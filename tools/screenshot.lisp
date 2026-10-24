;;;; screenshot.lisp -- a picture of the window, drawn by the window itself.
;;;;
;;;;   make screenshot INPUT=/some/folder/of/images
;;;;
;;;; Builds the real window over INPUT, identifies the images in it, selects
;;;; the first to review, and has AppKit draw the window into doc/window.png.
;;;; Nothing is captured from the screen, so it needs no Screen Recording
;;;; permission and shows nothing else that is open.  Nothing is written onto
;;;; the images: nothing is accepted.

(defpackage #:image-describe-screenshot
  (:use #:common-lisp)
  (:local-nicknames (#:id #:image-describe))
  (:export #:main))

(in-package #:image-describe-screenshot)

(defun settle (&optional (seconds 0.3d0))
  "Let the run loop lay out and draw what is on screen."
  (objc:invoke (objc:invoke "NSRunLoop" "currentRunLoop") "runUntilDate:"
               (objc:invoke "NSDate" "dateWithTimeIntervalSinceNow:" seconds)))

(defun write-png (view path)
  "Draw VIEW, as it would be on screen, into a PNG at PATH."
  (objc:invoke (objc:invoke view "window") "displayIfNeeded")
  (settle)
  (let* ((bounds (objc:invoke view "bounds"))
         (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds)))
    (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
    ;; NSBitmapImageFileTypePNG.
    (let ((data (objc:invoke rep "representationUsingType:properties:" 4
                             (objc:invoke "NSDictionary" "dictionary"))))
      (ensure-directories-exist path)
      (objc:invoke data "writeToFile:atomically:" (namestring path) t)
      (format t "~&wrote ~a~%" path))))

(defun main ()
  (let ((input (uiop:getenv "INPUT"))
        (id::*remember-settings* nil))
    (unless (and input (plusp (length input)))
      (error "Set INPUT to a folder of images."))
    (id::ensure-frameworks)
    (objc:with-autorelease-pool ()
      (objc.runloop:shared-application :activation-policy nil)
      ;; Light, whatever the Mac is set to, so that pictures taken on
      ;; different Macs match.
      (objc:invoke (objc.runloop:shared-application) "setAppearance:"
                   (objc:invoke "NSAppearance" "appearanceNamed:" "NSAppearanceNameAqua"))
      (let* ((controller (id::make-window-controller))
             (window (id::controller-window controller)))
        (objc:invoke window "orderFront:" nil)
        (objc:invoke (id::controller-folder-field controller) "setStringValue:"
                     (id::abbreviate-path (string-right-trim "/" input)))
        (objc:invoke (id::controller-recursive-checkbox controller) "setState:" 1)
        (id::rescan controller)
        (id::start-identifying controller)
        (loop while (id::collect-progress controller) do (sleep 0.05))
        (let ((row (or (id::next-review-row controller -1) 0)))
          (when (plusp (length (id::controller-entries controller)))
            (id::select-row controller row)
            (id::show-selection controller)
            (id::refresh-controls controller)))
        (objc:invoke window "makeFirstResponder:" (id::controller-table controller))
        (write-png (objc:invoke (objc:invoke window "contentView") "superview")
                   #p"doc/window.png")))))
