;;;; app.lisp -- the application: its delegate, its menu, and MAIN.

(in-package #:image-describe)

(defvar *controller* nil
  "The window's controller.  Held here because nothing in AppKit retains a
target, a data source or a delegate.")
(defvar *application-delegate* nil)

(objc:define-objc-class application-delegate ()
  ()
  (:objc-class-name "ImageDescribeApplicationDelegate"))

;;; One window, and closing it is quitting.
(objc:define-objc-method ("applicationShouldTerminateAfterLastWindowClosed:"
                          objc:objc-bool)
    ((self application-delegate) (application objc:objc-object-pointer))
  (declare (ignorable self application))
  t)

;;; Quitting with descriptions not yet accepted asks first.
(objc:define-objc-method ("applicationShouldTerminate:" (:unsigned :long))
    ((self application-delegate) (application objc:objc-object-pointer))
  (declare (ignorable self application))
  (handler-case (if (or (null *controller*)
                        (progn (commit-editing *controller*)
                               (confirm-discard *controller*)))
                    +ns-terminate-now+
                    +ns-terminate-cancel+)
    (error (condition)
      (note "applicationShouldTerminate: ~a" condition)
      +ns-terminate-now+)))

(objc:define-objc-method ("applicationWillTerminate:" :void)
    ((self application-delegate) (notification objc:objc-object-pointer))
  (declare (ignorable self notification))
  (handler-case (when *controller*
                  (stop-and-wait *controller*)
                  (save-settings *controller*)
                  (note "quit"))
    (error (condition) (note "applicationWillTerminate: ~a" condition))))

;;; The menu ----------------------------------------------------------------------
;;;
;;; A program with no nib has no menu bar until it makes one, and without an
;;; Edit menu there is no Paste in the fields.  Every target is nil: the
;;; responder chain finds the field for Paste, the window's delegate -- the
;;; controller -- for Identify and Accept, and the application for Quit.

(defun add-menu-item (menu title action key &optional modifiers)
  (let ((item (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                           "initWithTitle:action:keyEquivalent:"
                           title (objc:coerce-to-selector action) key)))
    (when modifiers
      (objc:invoke item "setKeyEquivalentModifierMask:" modifiers))
    (objc:invoke menu "addItem:" item)
    (objc:release item)
    item))

(defun add-submenu (main title items)
  "A top-level menu of ITEMS, each (TITLE ACTION [KEY [MODIFIERS]]) or
:SEPARATOR.  MODIFIERS replaces the Command a key has by itself: an upper
case KEY is Shift-Command already."
  (let ((menu (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" title))
        (holder (objc:invoke (objc:invoke "NSMenuItem" "alloc")
                             "initWithTitle:action:keyEquivalent:"
                             title (cffi:null-pointer) "")))
    (dolist (item items)
      (if (eq item :separator)
          (objc:invoke menu "addItem:" (objc:invoke "NSMenuItem" "separatorItem"))
          (destructuring-bind (item-title action &optional (key "") modifiers) item
            (add-menu-item menu item-title action key modifiers))))
    (objc:invoke holder "setSubmenu:" menu)
    (objc:invoke main "addItem:" holder)
    (objc:release holder)
    menu))

;;; 1572864 is Command and Option, NSEventModifierFlagCommand (1 << 20) and
;;; NSEventModifierFlagOption (1 << 19).
(defun install-menu ()
  (let ((main (objc:invoke (objc:invoke "NSMenu" "alloc") "initWithTitle:" "Main"))
        (application (objc.runloop:shared-application)))
    (add-submenu main "Image Describer"
                 '(("About Image Describer" "orderFrontStandardAboutPanel:")
                   :separator
                   ("Hide Image Describer" "hide:" "h")
                   ("Hide Others" "hideOtherApplications:" "h" 1572864)
                   :separator
                   ("Quit Image Describer" "terminate:" "q")))
    (add-submenu main "File"
                 '(("Choose Folder…" "chooseFolder:" "o")
                   :separator
                   ("Identify" "identify:" "r")
                   ("Identify Selected Again" "identifyAgain:" "R")
                   :separator
                   ("Accept" "acceptSelected:" "s")
                   ("Accept All" "acceptAll:" "S")
                   ("Remove Description" "removeDescription:")
                   :separator
                   ("Show in Finder" "showInFinder:" "r" 1572864)
                   :separator
                   ("Close" "performClose:" "w")))
    (add-submenu main "Edit"
                 '(("Undo" "undo:" "z")
                   ("Redo" "redo:" "Z")
                   :separator
                   ("Cut" "cut:" "x")
                   ("Copy" "copy:" "c")
                   ("Paste" "paste:" "v")
                   :separator
                   ("Select All" "selectAll:" "a")))
    (objc:invoke application "setWindowsMenu:"
                 (add-submenu main "Window"
                              '(("Minimize" "performMiniaturize:" "m")
                                ("Zoom" "performZoom:"))))
    (objc:invoke application "setMainMenu:" main)
    main))

;;; Starting ----------------------------------------------------------------------

(defun build-application ()
  "The controller, window, delegate and menu.  Answers the controller."
  (let ((controller (make-window-controller))
        (delegate (make-instance 'application-delegate)))
    (setf *controller* controller
          *application-delegate* delegate)
    (install-menu)
    (objc:invoke (objc.runloop:shared-application) "setDelegate:"
                 (objc:objc-object-pointer delegate))
    controller))

(defun env (name)
  (let ((value (uiop:getenv name)))
    (and value (plusp (length value)) value)))

(defun self-test-folder ()
  "The folder IMAGE_DESCRIBE_SELFTEST names, or NIL.

With it set, the application identifies every image in that folder and its
subfolders as soon as the window is up, logs each description, and quits.
With IMAGE_DESCRIBE_SELFTEST_ACCEPT=1 it accepts them all first, writing them
onto the images.  It is how the bundle is checked from a script, where nothing
can click:

  open -W --env IMAGE_DESCRIBE_SELFTEST=$PWD/samples \"build/Image Describer.app\"

The options remembered from the window are neither read nor changed."
  (env "IMAGE_DESCRIBE_SELFTEST"))

(defun start-self-test (controller folder)
  (setf *quit-when-done* t
        *accept-when-done* (equal (env "IMAGE_DESCRIBE_SELFTEST_ACCEPT") "1"))
  (objc:invoke (controller-folder-field controller) "setStringValue:" folder)
  (objc:invoke (controller-recursive-checkbox controller) "setState:" 1)
  (rescan controller)
  (note "self test: ~a: ~a" folder (status-line controller))
  (unless (start-identifying controller)
    (note "self test: did not start: ~a" (status-line controller))
    (setf (controller-confirmed-quit controller) t)
    (objc:invoke (objc.runloop:shared-application) "terminate:" nil)))

(defun main ()
  "The application's entry point.  Does not return."
  (setf *log* *error-output*)
  (handler-case
      (let ((self-test (self-test-folder))
            (controller nil))
        (when self-test
          (setf *remember-settings* nil))
        ;; How many images at once, when not the default: for measuring.
        (let ((workers (ignore-errors (parse-integer (or (env "IMAGE_DESCRIBE_WORKERS") "")))))
          (when (and workers (plusp workers))
            (setf *worker-count* workers)))
        (ensure-frameworks)
        (objc.runloop:shared-application)
        (note "~d image extensions; ~d workers" (length (image-extensions))
              (or *worker-count* (default-worker-count)))
        (setf controller (build-application))
        (show-window controller)
        (note "ready")
        (when self-test
          (start-self-test controller self-test)))
    (error (condition)
      (note "could not start: ~a" condition)
      (sb-ext:exit :code 1 :abort t)))
  (objc.runloop:run-cocoa-application))
