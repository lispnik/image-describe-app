;;;; ui-tests.lisp -- the real window, driven without a click.
;;;;
;;;; The window is built and never shown.  Its controls are set the way a click
;;;; or some typing would set them, the controller is told, and then the table
;;;; is asked what it shows.  Identifying runs on worker threads as in the
;;;; application, and the main thread's look at their progress, which the run
;;;; loop would make ten times a second, is made by hand: the run loop is not
;;;; running here.
;;;;
;;;; Most of these give the rows what Vision would have found rather than ask
;;;; it, so that they say what the window does with a finding whatever Vision
;;;; makes of a test picture.  One asks it for real.
;;;;
;;;; Skipped where there is no window server, which the count of skips says.

(in-package #:image-describe/tests)

(def-suite ui :in all-tests :description "The window.  Needs a window server.")
(in-suite ui)

(defun call-with-controller (function &key previews background)
  (let ((controller nil)
        (id::*remember-settings* nil)
        (id::*list-in-background* background))
    (unwind-protect
         (objc:with-autorelease-pool ()
           ;; No policy, so no Dock icon and no stolen focus.
           (objc.runloop:shared-application :activation-policy nil)
           (setf controller (id::make-window-controller :previews previews))
           (funcall function controller))
      (when controller
        ;; No worker left reading a folder about to be deleted.
        (id::close-window-controller controller)))))

(defmacro with-controller ((controller &rest options) &body body)
  "Run BODY with CONTROLLER a new window controller, or skip.  OPTIONS:
:PREVIEWS for the preview thread, :BACKGROUND to list folders on a thread."
  `(cond ((not (ignore-errors (id::ensure-frameworks) t))
          (skip "Objective-C runtime not available"))
         ((not (objc.runloop:window-server-p))
          (skip "no window server"))
         (t (call-with-controller (lambda (,controller) ,@body) ,@options))))

(defun target (controller) (objc:objc-object-pointer controller))
(defun table (controller) (id::controller-table controller))

(defun send (controller selector)
  "Send SELECTOR to the controller as a control would, with no sender."
  (objc:invoke (target controller) selector (cffi:null-pointer)))

(defun row-count (controller)
  (objc:invoke (target controller) "numberOfRowsInTableView:" (table controller)))

(defun cell (controller key row)
  (objc:invoke-into 'string (target controller)
                    "tableView:objectValueForTableColumn:row:"
                    (table controller)
                    (objc:invoke (table controller) "tableColumnWithIdentifier:" key)
                    row))

(defun column-cells (controller key)
  (loop for row below (row-count controller) collect (cell controller key row)))

(defun status-line (controller)
  (objc:invoke-into 'string (id::controller-status-label controller) "stringValue"))

(defun string-value (control)
  (objc:invoke-into 'string control "stringValue"))

(defun button-title (button)
  (objc:invoke-into 'string button "title"))

(defun enabled-p (control)
  (objc:invoke-bool control "isEnabled"))

(defun open-folder (controller folder &key recursive)
  (objc:invoke (id::controller-folder-field controller) "setStringValue:" folder)
  (objc:invoke (id::controller-recursive-checkbox controller) "setState:" (if recursive 1 0))
  (send controller "optionsChanged:"))

(defun select (controller &rest rows)
  (objc:invoke (table controller) "selectRowIndexes:byExtendingSelection:"
               (let ((set (objc:invoke "NSMutableIndexSet" "indexSet")))
                 (dolist (row rows set) (objc:invoke set "addIndex:" row)))
               nil)
  ;; As the table's notification would.
  (send controller "tableViewSelectionDidChange:"))

(defun type-description (controller text)
  "Type TEXT into the description field, as each key would tell the controller."
  (objc:invoke (id::controller-editor controller) "setStringValue:" text)
  (send controller "controlTextDidChange:"))

(defun pretend-identified (controller &rest labels-by-row)
  "Give each row the labels Vision would have found, and propose."
  (loop for labels in labels-by-row
        for entry across (id::controller-entries controller)
        do (setf (id:entry-analysis entry) (id:make-analysis :labels labels)
                 (id:entry-status entry) :identified)
           (id::propose controller entry))
  (objc:invoke (table controller) "reloadData")
  (id::refresh-controls controller))

(defun wait-for-identifying (controller)
  "Look at the workers' progress, as the run loop would, until they are done."
  (loop repeat 12000 while (id::collect-progress controller) do (sleep 0.01)))

(defparameter *dog* '(("animal" . 0.95d0) ("canine" . 0.95d0) ("dog" . 0.95d0)))
(defparameter *cat* '(("animal" . 0.9d0) ("cat" . 0.9d0) ("feline" . 0.9d0)
                      ("sofa" . 0.35d0)))

(test the-table-lists-the-folder-with-what-each-file-carries
  (with-temporary-directory (dir)
    (make-sample (path dir "b.png"))
    (make-sample (path dir "a.png"))
    (make-sample (path dir "sub" "c.png"))
    (write-text (path dir "readme.txt") "")
    (id:write-description (path dir "b.png") "Described before")
    (with-controller (controller)
      (open-folder controller dir)
      (is (equal '("a.png" "b.png") (column-cells controller "file")))
      (is (equal '("" "saved") (column-cells controller "status")))
      (is (equal '("" "Described before") (column-cells controller "description")))
      (is (search "2 images: 1 described, 1 not identified" (status-line controller)))
      (open-folder controller dir :recursive t)
      (is (equal '("a.png" "b.png" "sub/c.png") (column-cells controller "file"))))))

(test identify-proposes-and-writes-nothing
  (with-temporary-directory (dir)
    (let ((zebra (account-picture "Zebra")))
      (if zebra (copy-file zebra (path dir "a-zebra.heic")) (make-sample (path dir "a-zebra.png"))))
    (make-sample (path dir "b.png"))
    (write-text (path dir "c-broken.png") "no")
    (id:write-description (path dir "b.png") "Already described")
    (with-controller (controller)
      (open-folder controller dir)
      (send controller "identify:")
      (is (string= "Stop" (button-title (id::controller-identify-button controller))))
      (wait-for-identifying controller)
      (is (string= "Identify" (button-title (id::controller-identify-button controller))))
      (destructuring-bind (zebra described broken) (column-cells controller "status")
        (is (member zebra '("to review" "nothing found") :test #'string=))
        ;; A described image is not identified again unless asked.
        (is (string= "saved" described))
        (is (string= "error" broken)))
      (when (account-picture "Zebra")
        (is (search "Zebra" (cell controller "description" 0))))
      (is (null (id:read-description (id:entry-path (aref (id::controller-entries controller) 0)))))
      (is (string= "Already described" (id:read-description (path dir "b.png")))))))

(test return-accepts-and-goes-on-to-the-next
  (with-temporary-directory (dir)
    (dolist (name '("a.png" "b.png" "c.png")) (make-sample (path dir name)))
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog* *cat* *dog*)
      (is (equal '("to review" "to review" "to review") (column-cells controller "status")))
      (select controller 0)
      (is (string= "Dog, canine, animal" (string-value (id::controller-editor controller))))
      (is-true (enabled-p (id::controller-accept-button controller)))
      ;; Return, with the proposal as it is.
      (send controller "descriptionEntered:")
      (is (string= "Dog, canine, animal" (id:read-description (path dir "a.png"))))
      (is (equal '(1) (id::selected-rows controller)))
      ;; The next one, edited first.
      (type-description controller "Our cat, sofa")
      (is (string= "edited" (cell controller "status" 1)))
      (is (string= "Our cat, sofa" (cell controller "description" 1)))
      (send controller "descriptionEntered:")
      (is (string= "Our cat, sofa" (id:read-description (path dir "b.png"))))
      (is (equal '(2) (id::selected-rows controller)))
      (is (equal '("saved" "saved" "to review") (column-cells controller "status"))))))

(test accept-all-writes-every-proposal
  (with-temporary-directory (dir)
    (dolist (name '("a.png" "b.png" "c.png")) (make-sample (path dir name)))
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog* '() *cat*)
      (is (equal '("to review" "nothing found" "to review") (column-cells controller "status")))
      (send controller "acceptAll:")
      (is (equal '("saved" "nothing found" "saved") (column-cells controller "status")))
      (is (string= "Dog, canine, animal" (id:read-description (path dir "a.png"))))
      (is (null (id:read-description (path dir "b.png"))))
      (is (string= "Cat, feline, animal, sofa" (id:read-description (path dir "c.png"))))
      (is-false (enabled-p (id::controller-accept-all-button controller)))
      (is (search "2 described" (status-line controller))))))

(test the-minimum-confidence-changes-only-proposals
  (with-temporary-directory (dir)
    (dolist (name '("a.png" "b.png" "c.png")) (make-sample (path dir name)))
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *cat* *cat* *cat*)
      (select controller 1)
      (type-description controller "Typed")
      (select controller 2)
      (send controller "acceptSelected:")
      (objc:invoke (id::controller-confidence-slider controller) "setIntegerValue:" 50)
      (send controller "settingsChanged:")
      (is (string= "50%" (string-value (id::controller-confidence-label controller))))
      (is (equal '("Cat, feline, animal" "Typed" "Cat, feline, animal, sofa")
                 (column-cells controller "description")))
      (is (equal '("to review" "edited" "saved") (column-cells controller "status"))))))

(test identify-again-redoes-a-described-image-without-writing
  (let ((zebra (account-picture "Zebra")))
    (if (not zebra)
        (skip "No account pictures")
        (with-temporary-directory (dir)
          (copy-file zebra (path dir "zebra.heic"))
          (id:write-description (path dir "zebra.heic") "A horse")
          (with-controller (controller)
            (open-folder controller dir)
            ;; Nothing left to identify.
            (send controller "identify:")
            (is (null (id::controller-run controller)))
            (is (search "Identify Again" (status-line controller)))
            (select controller 0)
            (send controller "identifyAgain:")
            (wait-for-identifying controller)
            (is (string= "to review" (cell controller "status" 0)))
            (is (search "Zebra" (cell controller "description" 0)))
            (is (string= "A horse" (id:read-description (path dir "zebra.heic")))))))))

(test remove-description-takes-it-off
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (id:write-description (path dir "a.png") "Dog")
    (with-controller (controller)
      (open-folder controller dir)
      (select controller 0)
      (send controller "removeDescription:")
      (is (null (id:read-description (path dir "a.png"))))
      (is (equal '("") (column-cells controller "status")))
      (is (equal '("") (column-cells controller "description"))))))

(test listing-again-keeps-what-was-found
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (make-sample (path dir "sub" "b.png"))
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog*)
      (select controller 0)
      (type-description controller "Rex")
      (open-folder controller dir :recursive t)
      (is (equal '("a.png" "sub/b.png") (column-cells controller "file")))
      (is (equal '("Rex" "") (column-cells controller "description")))
      (is (equal '("edited" "") (column-cells controller "status"))))))

(test several-selected-are-accepted-each-with-its-own
  (with-temporary-directory (dir)
    (dolist (name '("a.png" "b.png" "c.png")) (make-sample (path dir name)))
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog* *cat* *dog*)
      (select controller 0 2)
      (is-false (enabled-p (id::controller-editor controller)))
      (is (search "2 images selected" (string-value (id::controller-title-label controller))))
      (send controller "acceptSelected:")
      (is (equal '("saved" "to review" "saved") (column-cells controller "status")))
      (is (string= "Dog, canine, animal" (id:read-description (path dir "c.png")))))))

(test a-locked-file-is-reported
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (sb-posix:chmod (path dir "a.png") #o444)
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog*)
      (send controller "acceptAll:")
      (is (equal '("error") (column-cells controller "status")))
      (is (search "could not be written" (status-line controller))))))

(test stop-leaves-images-that-take-too-long
  (with-temporary-directory (dir)
    (dolist (name '("a.png" "b.png" "c.png")) (make-sample (path dir name)))
    (with-controller (controller)
      (open-folder controller dir)
      (let ((release (sb-thread:make-semaphore)))
        (unwind-protect
             (progn
               ;; A run whose images never finish, as on a slow disk.
               (setf (id::controller-run controller)
                     (id:start-batch (id::controller-entries controller)
                                     (lambda (entry)
                                       (sb-thread:wait-on-semaphore release)
                                       (lambda () (setf (id:entry-status entry) :identified)))
                                     :begin #'id:begin-identifying :workers 2))
               (loop repeat 200 until (equal '("identifying…" "identifying…" "")
                                             (column-cells controller "status"))
                     do (sleep 0.01))
               (send controller "identify:")    ; Stop
               (is (string= "Stopping…" (button-title (id::controller-identify-button controller))))
               (let ((start (get-internal-real-time)))
                 (wait-for-identifying controller)
                 (is (< (/ (- (get-internal-real-time) start) internal-time-units-per-second) 5)))
               (is (null (id::controller-run controller)))
               (is (string= "Identify" (button-title (id::controller-identify-button controller))))
               (is (equal '("" "" "") (column-cells controller "status")))
               (is (search "2 images still being read were left" (status-line controller))))
          (sb-thread:signal-semaphore release 10))))))

;;; Listing and previews on threads of their own ---------------------------------

(defun pump (controller selector predicate &key (timeout 30))
  "Send SELECTOR to the controller, as the run loop would when a thread asks
for it, until PREDICATE is true or TIMEOUT seconds pass."
  (loop with deadline = (+ (get-internal-real-time) (* timeout internal-time-units-per-second))
        until (or (funcall predicate) (> (get-internal-real-time) deadline))
        do (sleep 0.02)
           (send controller selector))
  (funcall predicate))

(test the-folder-is-listed-off-the-main-thread
  (with-temporary-directory (dir)
    (dotimes (i 5) (make-sample (path dir (format nil "~d.png" i))))
    (with-controller (controller :background t)
      (open-folder controller dir)
      ;; Asked for, and not yet taken.
      (is (search "Listing" (status-line controller)))
      (is-true (pump controller "listingDone:" (lambda () (= 5 (row-count controller)))))
      (is (search "5 images" (status-line controller)))
      ;; An older listing's answer is not taken over a newer one's.
      (open-folder controller "/no/such/folder")
      (is (= 0 (row-count controller)))
      (sleep 0.3)
      (send controller "listingDone:")
      (is (= 0 (row-count controller))))))

(test the-preview-is-made-off-the-main-thread
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png") :width 640 :height 480)
    (with-controller (controller :previews t)
      (open-folder controller dir)
      (select controller 0)
      (is-true (pump controller "previewReady:"
                     (lambda () (not (null-object-p* (objc:invoke (id::controller-preview controller)
                                                                  "image"))))))
      ;; Kept: selecting it again shows it at once.
      (select controller)
      (is (null-object-p* (objc:invoke (id::controller-preview controller) "image")))
      (select controller 0)
      (is-false (null-object-p* (objc:invoke (id::controller-preview controller) "image"))))))

(defun null-object-p* (object)
  (or (null object) (cffi:null-pointer-p object)))

;;; Undo, revert, drops, tags ------------------------------------------------------

(defun undo (controller)
  (objc:invoke (id::controller-undo-manager controller) "undo"))

(defun redo (controller)
  (objc:invoke (id::controller-undo-manager controller) "redo"))

(test accepting-can-be-undone-and-redone
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (make-sample (path dir "b.png"))
    (id:write-description (path dir "b.png") "Before")
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog* *cat*)
      (send controller "acceptAll:")
      (is (string= "Dog, canine, animal" (id:read-description (path dir "a.png"))))
      (is (string= "Cat, feline, animal, sofa" (id:read-description (path dir "b.png"))))
      (undo controller)
      (is (null (id:read-description (path dir "a.png"))))
      (is (string= "Before" (id:read-description (path dir "b.png"))))
      (is (equal '("to review" "to review") (column-cells controller "status")))
      (redo controller)
      (is (string= "Dog, canine, animal" (id:read-description (path dir "a.png"))))
      (is (equal '("saved" "saved") (column-cells controller "status"))))))

(test removing-can-be-undone
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (id:write-description (path dir "a.png") "Dog")
    (with-controller (controller)
      (open-folder controller dir)
      (select controller 0)
      (send controller "removeDescription:")
      (is (null (id:read-description (path dir "a.png"))))
      (undo controller)
      (is (string= "Dog" (id:read-description (path dir "a.png"))))
      (is (equal '("saved") (column-cells controller "status"))))))

(test revert-goes-back-to-what-was-written-or-proposed
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (make-sample (path dir "b.png"))
    (id:write-description (path dir "b.png") "Written")
    (with-controller (controller)
      (open-folder controller dir)
      (pretend-identified controller *dog*)
      (select controller 0)
      (type-description controller "Typed")
      (is (string= "edited" (cell controller "status" 0)))
      (send controller "revertDescription:")
      (is (string= "Dog, canine, animal" (cell controller "description" 0)))
      (is (string= "to review" (cell controller "status" 0)))
      (select controller 1)
      (type-description controller "Typed over")
      (send controller "revertDescription:")
      (is (string= "Written" (cell controller "description" 1)))
      (is (string= "Written" (string-value (id::controller-editor controller)))))))

(test dropped-images-are-shown-in-their-folder
  (with-temporary-directory (dir)
    (make-sample (path dir "one" "a.png"))
    (make-sample (path dir "one" "b.png"))
    (make-sample (path dir "two" "c.png"))
    (with-controller (controller)
      ;; One folder's images: that folder, those selected.
      (id::open-paths controller (list (path dir "one" "b.png")))
      (is (string= (path (id:canonical-path dir) "one") (id::folder-path controller)))
      (is (equal '(1) (id::selected-rows controller)))
      ;; Two folders' images: the folder holding both, with subfolders.
      (id::open-paths controller (list (path dir "one" "a.png") (path dir "two" "c.png")))
      (is (string= (id:canonical-path dir) (id::folder-path controller)))
      (is (equal '("one/a.png" "one/b.png" "two/c.png") (column-cells controller "file")))
      (is (equal '(0 2) (id::selected-rows controller)))
      ;; A folder is the folder.
      (id::open-paths controller (list (path dir "two")))
      (is (equal '("c.png") (column-cells controller "file"))))))

(test tags-are-written-when-chosen
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (with-controller (controller)
      (open-folder controller dir)
      (id::set-checked (id::controller-tags-checkbox controller) t)
      (pretend-identified controller *dog*)
      (send controller "acceptAll:")
      (is (equal '("Dog") (id::read-tags (path dir "a.png"))))
      (undo controller)
      (is (null (id::read-tags (path dir "a.png")))))))
