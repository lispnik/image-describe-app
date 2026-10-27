;;;; window.lisp -- the window: the folder, the list of images, and the
;;;; description of the one selected.
;;;;
;;;; One controller is the target of every control, the table's data source
;;;; and delegate, and the description field's delegate.  What the table shows
;;;; is a vector of ENTRYs (batch.lisp).
;;;;
;;;; The work goes: choose a folder, and the images in it are listed with any
;;;; description they already carry; Identify asks Vision about each of the
;;;; rest, on worker threads, and proposes a description; select one to see it
;;;; and edit it, and Accept (or Return in the field) writes it onto the file
;;;; and moves on to the next to review.  Accept All writes every proposal.
;;;;
;;;; The workers touch neither AppKit nor any entry's description: the main
;;;; thread looks ten times a second at what they have finished, proposes the
;;;; descriptions, and redraws.

(in-package #:image-describe)

(objc:define-objc-class window-controller ()
  ((window :initform nil :accessor controller-window)
   (folder-field :initform nil :accessor controller-folder-field)
   (recursive-checkbox :initform nil :accessor controller-recursive-checkbox)
   (text-checkbox :initform nil :accessor controller-text-checkbox)
   (places-checkbox :initform nil :accessor controller-places-checkbox)
   (tags-checkbox :initform nil :accessor controller-tags-checkbox)
   (get-info-checkbox :initform nil :accessor controller-get-info-checkbox)
   (confidence-slider :initform nil :accessor controller-confidence-slider)
   (confidence-label :initform nil :accessor controller-confidence-label)
   (table :initform nil :accessor controller-table)
   (preview :initform nil :accessor controller-preview)
   (preview-path :initform nil :accessor controller-preview-path
                 :documentation "The image the preview shows, or is being made of.")
   (previewer :initform nil :accessor controller-previewer
              :documentation "The thread making previews (preview.lisp).")
   (title-label :initform nil :accessor controller-title-label)
   (editor :initform nil :accessor controller-editor)
   (editor-row :initform nil :accessor controller-editor-row
               :documentation "The row the description field is editing, or NIL.")
   (accept-button :initform nil :accessor controller-accept-button)
   (again-button :initform nil :accessor controller-again-button)
   (revert-button :initform nil :accessor controller-revert-button)
   (status-label :initform nil :accessor controller-status-label)
   (identify-button :initform nil :accessor controller-identify-button)
   (accept-all-button :initform nil :accessor controller-accept-all-button)
   (option-controls :initform '() :accessor controller-option-controls
                    :documentation "What cannot be changed while identifying.")
   (entries :initform #() :accessor controller-entries
            :documentation "The table's rows.")
   (run :initform nil :accessor controller-run
        :documentation "The BATCH-RUN identifying, while there is one.")
   (stopping :initform nil :accessor controller-stopping
             :documentation "When Stop was pressed, as an internal real time, while the
images in hand are being finished.")
   (message :initform nil :accessor controller-message
            :documentation "Something to say in place of the count of images.")
   (confirmed-quit :initform nil :accessor controller-confirmed-quit
                   :documentation "Losing the unaccepted descriptions was agreed to.")
   (listing :initform nil :accessor controller-listing
            :documentation "The folder is being listed, by a thread of its own.")
   (listing-generation :initform 0 :accessor controller-listing-generation
                       :documentation "Counts listings, so that an old one's answer is not taken.")
   (listing-result :initform nil :accessor controller-listing-result
                   :documentation "(generation entries error), from the listing thread.")
   (listing-lock :initform (sb-thread:make-mutex :name "listing") :accessor controller-listing-lock)
   (select-after-listing :initform '() :accessor controller-select-after-listing
                         :documentation "Paths to select once the folder is listed: dropped files.")
   (undo-manager :initform nil :accessor controller-undo-manager)
   (undo-actions :initform (make-hash-table) :accessor controller-undo-actions
                 :documentation "What each registered undo does, by the number given NSUndoManager.")
   (undo-count :initform 0 :accessor controller-undo-count))
  (:objc-class-name "ImageDescribeWindowController"))

(defmacro define-controller-method ((selector result-type &key on-error)
                                    (&rest arguments) &body body)
  "A method of the controller.  Nothing may unwind into AppKit, so whatever
BODY signals is logged and ON-ERROR is answered instead."
  `(objc:define-objc-method (,selector ,result-type)
       ((self window-controller) ,@arguments)
     (declare (ignorable self ,@(mapcar #'first arguments)))
     (handler-case (progn ,@body)
       (error (condition)
         (note "~a: ~a" ,selector condition)
         ,on-error))))

(defun controller-running (controller)
  (and (controller-run controller) t))

;;; Settings ------------------------------------------------------------------------
;;;
;;; Kept in the user defaults, so the window opens as it was left.

(defvar *remember-settings* t
  "Whether the options are read from and written to the user defaults.  The
tests bind it to NIL, so as not to change the settings of whoever runs them.")

(defun user-defaults ()
  (objc:invoke "NSUserDefaults" "standardUserDefaults"))

(defun default-string (key)
  (and *remember-settings*
       (objc:invoke-into 'string (user-defaults) "stringForKey:" key)))

(defun default-present-p (key)
  (and *remember-settings*
       (not (null-object-p (objc:invoke (user-defaults) "objectForKey:" key)))))

(defun default-bool (key default)
  (if (default-present-p key)
      (objc:invoke-bool (user-defaults) "boolForKey:" key)
      default))

(defun default-integer (key default)
  (if (default-present-p key)
      (objc:invoke (user-defaults) "integerForKey:" key)
      default))

(defun save-settings (controller)
  (when *remember-settings*
    (let ((defaults (user-defaults)))
      (objc:invoke defaults "setObject:forKey:" (folder-path controller) "Folder")
      (objc:invoke defaults "setBool:forKey:" (recursive-p controller) "IncludeSubfolders")
      (objc:invoke defaults "setBool:forKey:" (read-text-p controller) "ReadText")
      (objc:invoke defaults "setBool:forKey:" (places-p controller) "PlaceNames")
      (objc:invoke defaults "setBool:forKey:" (tags-p controller) "FinderTags")
      (objc:invoke defaults "setBool:forKey:" (get-info-p controller) "GetInfoComments")
      (objc:invoke defaults "setInteger:forKey:" (confidence-percent controller)
                   "MinimumConfidence"))))

;;; Reading the options ---------------------------------------------------------------

(defun field-string (field)
  (or (objc:invoke-into 'string field "stringValue") ""))

(defun expand-path (string)
  "STRING as a native path: trimmed, ~ expanded, and with no trailing slash."
  (let ((path (string-trim '(#\Space #\Tab #\Newline) string)))
    (when (and (plusp (length path)) (char= #\~ (char path 0))
               (or (= 1 (length path)) (char= #\/ (char path 1))))
      (setf path (concatenate 'string
                              (string-right-trim "/" (native (user-homedir-pathname)))
                              (subseq path 1))))
    (if (and (> (length path) 1) (char= #\/ (char path (1- (length path)))))
        (string-right-trim "/" path)
        path)))

(defun abbreviate-path (path)
  "PATH with the home folder written ~, as the folder field shows it."
  (let ((home (string-right-trim "/" (native (user-homedir-pathname)))))
    (if (and (>= (length path) (length home))
             (string= home path :end2 (length home))
             (or (= (length path) (length home)) (char= #\/ (char path (length home)))))
        (concatenate 'string "~" (subseq path (length home)))
        path)))

(defun folder-path (controller)
  (expand-path (field-string (controller-folder-field controller))))

(defun checked-p (checkbox)
  (= 1 (objc:invoke checkbox "state")))

(defun recursive-p (controller)
  (checked-p (controller-recursive-checkbox controller)))

(defun read-text-p (controller)
  (checked-p (controller-text-checkbox controller)))

(defun places-p (controller)
  (checked-p (controller-places-checkbox controller)))

(defun tags-p (controller)
  (checked-p (controller-tags-checkbox controller)))

(defun get-info-p (controller)
  (checked-p (controller-get-info-checkbox controller)))

(defun set-checked (checkbox on)
  (objc:invoke checkbox "setState:" (if on 1 0)))

(defun confidence-percent (controller)
  (objc:invoke (controller-confidence-slider controller) "integerValue"))

(defun minimum-confidence (controller)
  (/ (confidence-percent controller) 100.0))

(defun directory-exists-p (path)
  (and (plusp (length path))
       (uiop:directory-exists-p (directory-name path))
       t))

;;; Rows ----------------------------------------------------------------------------

(defun entry-at (controller row)
  (let ((entries (controller-entries controller)))
    (and (integerp row) (< -1 row (length entries)) (aref entries row))))

(defun selected-rows (controller)
  "The rows selected, in order."
  (let* ((set (objc:invoke (controller-table controller) "selectedRowIndexes"))
         (count (objc:invoke set "count")))
    (loop with index = (objc:invoke set "firstIndex")
          repeat count
          collect index
          do (setf index (objc:invoke set "indexGreaterThanIndex:" index)))))

(defun select-row (controller row)
  (let ((table (controller-table controller)))
    (objc:invoke table "selectRowIndexes:byExtendingSelection:"
                 (objc:invoke "NSIndexSet" "indexSetWithIndex:" row) nil)
    (objc:invoke table "scrollRowToVisible:" row)))

(defun rows-in-state (controller &rest states)
  (loop for entry across (controller-entries controller)
        for row from 0
        when (member (entry-state entry) states) collect row))

(defun count-state (controller &rest states)
  (count-if (lambda (entry) (member (entry-state entry) states))
            (controller-entries controller)))

(defun pending-rows (controller)
  "The rows Identify works on: never identified, or failed."
  (rows-in-state controller nil :error))

(defun review-rows (controller)
  "The rows with a description not yet written."
  (rows-in-state controller :proposed :edited))

(defun next-review-row (controller after)
  "The first row after AFTER, going round, with a description to review."
  (let* ((entries (controller-entries controller))
         (count (length entries)))
    (loop for step from 1 below (1+ count)
          for row = (mod (+ after step) count)
          when (member (entry-state (aref entries row)) '(:proposed :edited))
            return row)))

;;; The status line and the buttons ------------------------------------------------

(defun status-line (controller)
  (let* ((entries (controller-entries controller))
         (total (length entries))
         (saved (count-state controller :saved))
         (review (count-state controller :proposed :edited))
         (failed (count-state controller :error))
         (nothing (count-state controller :nothing))
         (waiting (count-state controller nil :identifying)))
    (cond ((controller-message controller))
          ((controller-listing controller)
           "Listing the images…")
          ((controller-stopping controller)
           "Stopping: finishing the images in hand…")
          ((controller-running controller)
           (let ((run (controller-run controller)))
             (format nil "Identifying ~d of ~d…"
                     (min (length (run-indices run)) (1+ (count-done controller)))
                     (length (run-indices run)))))
          ((zerop (length (folder-path controller)))
           "Choose a folder of images to describe.")
          ((not (directory-exists-p (folder-path controller)))
           "The folder does not exist.")
          ((zerop total)
           (format nil "No images in this folder~:[~; or its subfolders~]." (recursive-p controller)))
          (t
           (format nil "~d image~:p: ~{~a~^, ~}."
                   total
                   (remove nil
                           (list (and (plusp saved) (format nil "~d described" saved))
                                 (and (plusp review) (format nil "~d to review" review))
                                 (and (plusp waiting) (format nil "~d not identified" waiting))
                                 (and (plusp nothing) (format nil "~d with nothing found" nothing))
                                 (and (plusp failed) (format nil "~d failed" failed)))))))))

(defun count-done (controller)
  "How many of the run's images are finished."
  (let ((run (controller-run controller)))
    (if run
        (count-if (lambda (index)
                    (member (entry-status (aref (controller-entries controller) index))
                            '(:identified :error)))
                  (run-indices run))
        0)))

(defun refresh-controls (controller)
  "Make the controls agree with the state: the status line, the buttons'
titles, and which can be used."
  (let ((running (controller-running controller))
        (selected (selected-rows controller)))
    (objc:invoke (controller-status-label controller) "setStringValue:" (status-line controller))
    (objc:invoke (controller-identify-button controller) "setTitle:"
                 (cond ((controller-stopping controller) "Stopping…")
                       (running "Stop")
                       (t "Identify")))
    (objc:invoke (controller-identify-button controller) "setEnabled:"
                 (and (not (controller-stopping controller))
                      (or running (pending-rows controller))
                      t))
    (objc:invoke (controller-accept-all-button controller) "setEnabled:"
                 (and (review-rows controller) t))
    (objc:invoke (controller-accept-button controller) "setEnabled:"
                 (and (some (lambda (row)
                              (member (entry-state (entry-at controller row)) '(:proposed :edited)))
                            selected)
                      t))
    (objc:invoke (controller-again-button controller) "setEnabled:"
                 (and (not running) selected t))
    (objc:invoke (controller-revert-button controller) "setEnabled:"
                 (let ((entry (and (= 1 (length selected)) (entry-at controller (first selected)))))
                   (and entry (member (entry-state entry) '(:edited :proposed)) t)))
    (dolist (control (controller-option-controls controller))
      (objc:invoke control "setEnabled:" (not running)))))

;;; The folder ------------------------------------------------------------------------

(defvar *list-in-background* t
  "Whether the folder is listed on a thread of its own.  The tests list it
on theirs, so that the table is filled when they look.")

(defun rescan (controller &key select)
  "List the images in the folder again, on a thread of its own, and then
select those of SELECT, a list of paths.  An image listed before keeps what
was found in it and any description being reviewed; the others are read from
their files."
  (let ((folder (folder-path controller))
        (recursive (recursive-p controller))
        (generation (incf (controller-listing-generation controller)))
        (target (objc:objc-object-pointer controller)))
    (setf (controller-select-after-listing controller) select
          (controller-message controller) nil)
    (cond ((not (directory-exists-p folder))
           (setf (controller-listing controller) nil)
           (apply-listing controller #()))
          ((not *list-in-background*)
           (apply-listing controller (entries-from-directory folder :recursive recursive)))
          (t
           (setf (controller-listing controller) t)
           (refresh-controls controller)
           (start-listing folder :recursive recursive
                                 :on-done (lambda (entries error)
                                            (sb-thread:with-mutex ((controller-listing-lock controller))
                                              (setf (controller-listing-result controller)
                                                    (list generation entries error)))
                                            (objc:invoke target "performSelectorOnMainThread:withObject:waitUntilDone:"
                                                         (objc:coerce-to-selector "listingDone:")
                                                         nil nil)))))
    controller))

(defun take-listing (controller)
  "The entries the listing thread found, and its error, if it has finished
the latest listing."
  (let ((result (sb-thread:with-mutex ((controller-listing-lock controller))
                  (shiftf (controller-listing-result controller) nil))))
    (when (and result (= (first result) (controller-listing-generation controller)))
      (values (or (second result) #()) (third result) t))))

(define-controller-method ("listingDone:" :void) ((argument objc:objc-object-pointer))
  (multiple-value-bind (entries error found) (take-listing self)
    (when found
      (setf (controller-listing self) nil)
      (apply-listing self entries)
      (when error
        (setf (controller-message self) (format nil "The folder could not be listed: ~a" error))
        (refresh-controls self)))))

(defun apply-listing (controller found)
  "Make FOUND, entries just listed, the table's rows, keeping what is known
of each image listed before."
  (let ((old (make-hash-table :test #'equal)))
    (loop for entry across (controller-entries controller)
          do (setf (gethash (entry-path entry) old) entry))
    (setf (controller-entries controller)
          (map 'vector (lambda (entry)
                         (let ((before (gethash (entry-path entry) old)))
                           (cond ((null before) entry)
                                 (t
                                  ;; What the file carries now, whatever was known.
                                  (setf (entry-saved before) (entry-saved entry)
                                        (entry-relative before) (entry-relative entry))
                                  (when (and (not (entry-edited before))
                                             (not (eq :identified (entry-status before))))
                                    (setf (entry-description before) (entry-saved entry)))
                                  before))))
               found))
    (let ((table (controller-table controller))
          (select (controller-select-after-listing controller)))
      (objc:invoke table "reloadData")
      (objc:invoke table "deselectAll:" nil)
      (when select
        (let ((set (objc:invoke "NSMutableIndexSet" "indexSet")))
          (loop for entry across (controller-entries controller)
                for row from 0
                when (member (entry-path entry) select :test #'string=)
                  do (objc:invoke set "addIndex:" row))
          (objc:invoke table "selectRowIndexes:byExtendingSelection:" set nil)
          (when (plusp (objc:invoke set "count"))
            (objc:invoke table "scrollRowToVisible:" (objc:invoke set "firstIndex")))))
      (setf (controller-select-after-listing controller) '()))
    (show-selection controller)
    (refresh-controls controller)
    controller))

(defun options-changed (controller)
  (rescan controller)
  (save-settings controller))

(defun choose-directory (&key prompt message start)
  "Ask for one folder with an open panel.  Answers its path, or NIL."
  (let ((panel (objc:invoke "NSOpenPanel" "openPanel")))
    (objc:invoke panel "setCanChooseDirectories:" t)
    (objc:invoke panel "setCanChooseFiles:" nil)
    (objc:invoke panel "setAllowsMultipleSelection:" nil)
    (objc:invoke panel "setPrompt:" prompt)
    (objc:invoke panel "setMessage:" message)
    (when (directory-exists-p (or start ""))
      (objc:invoke panel "setDirectoryURL:" (objc:invoke "NSURL" "fileURLWithPath:" start)))
    (when (= +ns-modal-response-ok+ (objc:invoke panel "runModal"))
      (let ((urls (objc:invoke panel "URLs")))
        (and (plusp (objc:invoke urls "count"))
             (objc:invoke-into 'string (objc:invoke urls "objectAtIndex:" 0) "path"))))))

(defun set-folder (controller path)
  (objc:invoke (controller-folder-field controller) "setStringValue:" (abbreviate-path path))
  (options-changed controller))

(define-controller-method ("chooseFolder:" :void) ((sender objc:objc-object-pointer))
  (unless (controller-running self)
    (let ((path (choose-directory :prompt "Choose" :start (folder-path self)
                                  :message "Choose the folder of images to describe.")))
      (when path (set-folder self path)))))

(define-controller-method ("optionsChanged:" :void) ((sender objc:objc-object-pointer))
  (unless (controller-running self)
    (options-changed self)))

;;; How descriptions are proposed -------------------------------------------------------
;;;
;;; The minimum confidence and whether text is included change the proposals
;;; at once, without identifying again: each entry keeps all Vision found.
;;; Only proposals change; what was typed or written stays.

(defun propose (controller entry)
  (propose-description entry :minimum-confidence (minimum-confidence controller)
                             :include-text (read-text-p controller)
                             :include-place (places-p controller)))

(defun look-up-places (controller entries)
  "Find the place names of ENTRIES that have a position and no place yet;
each, when it comes, goes into the proposal."
  (dolist (entry entries)
    (let* ((analysis (entry-analysis entry))
           (location (and analysis (analysis-location analysis))))
      (when (and location (null (analysis-place analysis)))
        (lookup-place (car location) (cdr location)
                      (lambda (name)
                        (when name
                          (setf (analysis-place analysis) name)
                          (when (member (entry-state entry) '(:proposed :nothing))
                            (propose controller entry))
                          (objc:invoke (controller-table controller) "reloadData")
                          (when (eq entry (entry-at controller (controller-editor-row controller)))
                            (show-selection controller)))))))))

(defun show-confidence (controller)
  (objc:invoke (controller-confidence-label controller) "setStringValue:"
               (format nil "~d%" (confidence-percent controller))))

(defun settings-changed (controller)
  (show-confidence controller)
  (when (places-p controller)
    (look-up-places controller (coerce (controller-entries controller) 'list)))
  (loop for entry across (controller-entries controller)
        when (member (entry-state entry) '(:proposed :nothing))
          do (propose controller entry))
  (objc:invoke (controller-table controller) "reloadData")
  (show-selection controller)
  (refresh-controls controller)
  (save-settings controller))

(define-controller-method ("settingsChanged:" :void) ((sender objc:objc-object-pointer))
  (settings-changed self))

;;; Identifying ---------------------------------------------------------------------

(defvar *quit-when-done* nil
  "Whether the application quits when identifying ends: the self test.")

(defvar *accept-when-done* nil
  "Whether everything proposed is accepted when identifying ends: the self test.")

(defparameter +progress-interval+ 0.1d0
  "Seconds between the main thread's looks at what the workers have done.")

(defun schedule-progress (controller)
  (objc:invoke (objc:objc-object-pointer controller)
               "performSelector:withObject:afterDelay:inModes:"
               (objc:coerce-to-selector "identificationProgress:")
               nil +progress-interval+ +run-loop-modes+))

(defun start-identifying (controller &optional rows)
  "Identify ROWS, or every row not identified yet.  True if it began."
  (let ((rows (or rows (pending-rows controller)))
        (read-text (read-text-p controller)))
    (setf (controller-message controller)
          (cond ((not (directory-exists-p (folder-path controller)))
                 "Choose a folder that exists.")
                ((zerop (length (controller-entries controller)))
                 "There are no images to identify.")
                ((null rows)
                 "Every image has a description. Select some and choose Identify Again to redo them.")))
    (cond ((controller-message controller)
           (refresh-controls controller)
           nil)
          (t
           (save-settings controller)
           ;; Asked for again, so what was typed for them goes.
           (dolist (row rows)
             (setf (entry-edited (entry-at controller row)) nil))
           (note "identifying ~d image~:p in ~a, ~d at a time"
                 (length rows) (folder-path controller)
                 (min (length rows) (or *worker-count* (default-worker-count))))
           (setf (controller-run controller)
                 (start-batch (controller-entries controller)
                              (lambda (entry)
                                (identification entry :read-text read-text))
                              :begin #'begin-identifying
                              :indices rows)
                 (controller-stopping controller) nil)
           (show-selection controller)
           (refresh-controls controller)
           (schedule-progress controller)
           t))))

(defun stop-identifying (controller)
  "Hand out no more images.  The run ends when the ones in hand are done."
  (when (controller-run controller)
    (stop-batch (controller-run controller))
    (setf (controller-stopping controller) (get-internal-real-time))
    (note "stopping")
    (refresh-controls controller)))

(defparameter +stop-grace+ 2
  "Seconds Stop waits for the images in hand before leaving them.")

(defun abandon-identifying (controller)
  "Leave the images still in hand: their rows are as they were before, and
what the workers find in them is thrown away.  The workers only read, so
nothing is left half written."
  (let ((left (abandon-batch (controller-run controller))))
    (dolist (index left)
      (let ((entry (aref (controller-entries controller) index)))
        (setf (entry-status entry) (and (entry-analysis entry) :identified))))
    (when left
      (note "left ~d image~:p still being read" (length left))
      (setf (controller-message controller)
            (format nil "Stopped. ~d image~:p still being read ~:*~[~;was~:;were~] left unidentified."
                    (length left))))
    left))

(defun finish-identifying (controller)
  (setf (controller-run controller) nil
        (controller-stopping controller) nil)
  (note "done: ~d to review, ~d with nothing found, ~d failed"
        (count-state controller :proposed :edited) (count-state controller :nothing)
        (count-state controller :error))
  (when *accept-when-done*
    (accept-rows controller (review-rows controller)))
  (objc:invoke (controller-table controller) "reloadData")
  (show-selection controller)
  (refresh-controls controller)
  (when *quit-when-done*
    (setf (controller-confirmed-quit controller) t)
    (objc:invoke (objc.runloop:shared-application) "terminate:" nil)))

(defun collect-progress (controller)
  "Propose descriptions for what the workers have finished since the last
look, and end the run when they all have.  Answers true while it goes on."
  (let ((run (controller-run controller)))
    (if (null run)
        nil
        ;; Whether they are done first: what they finished before is then
        ;; all in TAKE-FINISHED.
        (let* ((done (or (batch-done-p run)
                         ;; Stop waits a little for the images in hand: one
                         ;; on a slow disk may take minutes.
                         (and (controller-stopping controller)
                              (> (- (get-internal-real-time) (controller-stopping controller))
                                 (* +stop-grace+ internal-time-units-per-second))
                              (progn (abandon-identifying controller) t))))
               (finished (take-finished run))
               (entries (controller-entries controller)))
          (when (places-p controller)
            (look-up-places controller (mapcar (lambda (index) (aref entries index)) finished)))
          (dolist (index finished)
            (let ((entry (aref entries index)))
              (propose controller entry)
              (note "~a: ~a~@[: ~a~]" (state-text (entry-state entry))
                    (entry-path entry)
                    (or (entry-message entry) (entry-description entry)))))
          (let ((table (controller-table controller)))
            ;; The rows being identified change too, not only the finished.
            (objc:invoke table "reloadData")
            (when (and finished (null (selected-rows controller)))
              (objc:invoke table "scrollRowToVisible:" (reduce #'max finished))))
          (when (intersection finished (selected-rows controller))
            (show-selection controller))
          (cond (done
                 (finish-identifying controller)
                 nil)
                (t
                 (refresh-controls controller)
                 t))))))

(define-controller-method ("identificationProgress:" :void) ((argument objc:objc-object-pointer))
  (when (collect-progress self)
    (schedule-progress self)))

(defun stop-and-wait (controller &key (timeout 1))
  "Stop identifying, for quitting: the images in hand are given TIMEOUT
seconds, and then left.  The workers only read, so quitting while one is
inside IM or Vision leaves nothing half written."
  (let ((run (controller-run controller)))
    (when run
      (stop-batch run)
      (unless (wait-for-batch run :timeout timeout)
        (abandon-batch run)
        (note "quit with images still being read")))))

(define-controller-method ("identify:" :void) ((sender objc:objc-object-pointer))
  ;; So that a folder still being typed is taken before it is read.
  (commit-editing self)
  (cond ((controller-stopping self))
        ((controller-running self) (stop-identifying self))
        (t (start-identifying self))))

(define-controller-method ("identifyAgain:" :void) ((sender objc:objc-object-pointer))
  (unless (controller-running self)
    (let ((rows (selected-rows self)))
      (if rows
          (start-identifying self rows)
          (progn (setf (controller-message self) "Select the images to identify again.")
                 (refresh-controls self))))))

;;; Accepting ---------------------------------------------------------------------

;;; Undo ----------------------------------------------------------------------------
;;;
;;; Accept and Remove Description change files, so Edit > Undo puts them back:
;;; each takes a snapshot of the rows it changes, and undoing restores the
;;; snapshots and writes what the files carried before, taking a snapshot of
;;; its own for Redo.  NSUndoManager keeps a number; what it means is here.

(defun snapshot (entry)
  (list entry (entry-saved entry) (entry-description entry) (entry-edited entry)
        (entry-status entry) (entry-analysis entry) (entry-message entry)))

(defun restore-snapshots (controller snapshots)
  "Put each row of SNAPSHOTS back as it was, and its file."
  (let ((changed '()))
    (dolist (snapshot snapshots)
      (destructuring-bind (entry saved description edited status analysis message) snapshot
        (let ((file-changed (not (equal saved (entry-saved entry)))))
          (setf (entry-saved entry) saved (entry-description entry) description
                (entry-edited entry) edited (entry-status entry) status
                (entry-analysis entry) analysis (entry-message entry) message)
          (when file-changed
            (handler-case
                (progn (write-description (entry-path entry) (or saved "")
                                          :tags (tags-for controller saved))
                       (push entry changed))
              (error (condition)
                (setf (entry-message entry) (condition-text condition)
                      (entry-status entry) :error)))))))
    (tell-finder controller (nreverse changed))
    (objc:invoke (controller-table controller) "reloadData")
    (show-selection controller)
    (refresh-controls controller)))

(defun register-undo (controller name function)
  (let ((manager (controller-undo-manager controller))
        (id (incf (controller-undo-count controller))))
    (when manager
      (setf (gethash id (controller-undo-actions controller)) function)
      (objc:invoke manager "registerUndoWithTarget:selector:object:"
                   (objc:objc-object-pointer controller)
                   (objc:coerce-to-selector "performUndo:")
                   (objc:invoke "NSNumber" "numberWithLong:" id))
      (objc:invoke manager "setActionName:" name))))

(defun undoable (controller name snapshots)
  "Register, as NAME, putting SNAPSHOTS back; doing so registers the redo."
  (register-undo controller name
                 (lambda ()
                   (let ((now (mapcar (lambda (snapshot) (snapshot (first snapshot))) snapshots)))
                     (restore-snapshots controller snapshots)
                     (undoable controller name now)))))

(defmacro with-undo ((controller name entries) &body body)
  "Run BODY, which changes ENTRIES, as one undoable action called NAME."
  (let ((snapshots (gensym)))
    `(let ((,snapshots (mapcar #'snapshot (remove nil ,entries))))
       (multiple-value-prog1 (progn ,@body)
         (undoable ,controller ,name ,snapshots)))))

(define-controller-method ("performUndo:" :void) ((number objc:objc-object-pointer))
  (let* ((id (objc:invoke number "longValue"))
         (function (gethash id (controller-undo-actions self))))
    (remhash id (controller-undo-actions self))
    (when function (funcall function))))

(define-controller-method ("windowWillReturnUndoManager:" objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((window objc:objc-object-pointer))
  (or (controller-undo-manager self) (cffi:null-pointer)))

(defun commit-editing (controller)
  "End any typing in the window, so that what was typed is in its entry."
  (objc:invoke (controller-window controller) "makeFirstResponder:" (cffi:null-pointer)))

(define-controller-method ("acceptSelected:" :void) ((sender objc:objc-object-pointer))
  (accept-selected self))

(defun accept-selected (controller &key (advance t))
  "Accept the selected rows.  With ADVANCE and one row selected, select the
next one to review, so that a folder can be gone through with Return alone."
  (let ((rows (selected-rows controller)))
    (accept-rows controller rows)
    (let ((next (and advance (= 1 (length rows)) (next-review-row controller (first rows)))))
      (if next
          (select-row controller next)
          (show-selection controller)))
    (refresh-controls controller)))

(define-controller-method ("acceptAll:" :void) ((sender objc:objc-object-pointer))
  (accept-rows self (review-rows self))
  (show-selection self)
  (refresh-controls self))

(defun tags-for (controller description)
  "The Finder tags to write with DESCRIPTION: its first keyword, if tags
are chosen; else leave the tags as they are."
  (if (tags-p controller)
      (let ((first (first (description-keywords (or description "")))))
        (and first (list first)))
      :keep))

(defun tell-finder (controller entries)
  "With Get Info comments chosen, have Finder set ENTRIES' comments to what
they carry.  If Finder may not be told, say why and stop asking."
  (when (and entries (get-info-p controller))
    (handler-case
        (set-finder-comments (mapcar (lambda (entry) (cons (entry-path entry) (entry-saved entry)))
                                     entries))
      (error (condition)
        (note "Get Info: ~a" condition)
        (set-checked (controller-get-info-checkbox controller) nil)
        (save-settings controller)
        (setf (controller-message controller)
              (format nil "~a. Show descriptions in Get Info is off." (condition-text condition)))))))

(defun accept-rows (controller rows)
  "Write the descriptions of ROWS onto their files.  Answers how many were.
Undoable."
  (let ((written '()) (failed 0))
    (with-undo (controller "Accept" (mapcar (lambda (row) (entry-at controller row)) rows))
      (dolist (row rows)
        (let ((entry (entry-at controller row)))
          (when (and entry (member (entry-state entry) '(:proposed :edited :saved)))
            (if (accept-entry entry :tags (tags-for controller (entry-description entry)))
                (progn (push entry written)
                       (note "saved: ~a: ~a" (entry-path entry) (entry-saved entry)))
                (progn (incf failed)
                       (note "not saved: ~a: ~a" (entry-path entry) (entry-message entry))))))))
    (setf (controller-message controller)
          (and (plusp failed)
               (format nil "~d description~:p could not be written — the file~:p may be locked or read-only."
                       failed)))
    (tell-finder controller (reverse written))
    (objc:invoke (controller-table controller) "reloadData")
    (length written)))

(define-controller-method ("removeDescription:" :void) ((sender objc:objc-object-pointer))
  (let ((failed 0) (removed '())
        (entries (remove-if (lambda (entry) (or (null entry) (eq (entry-state entry) :identifying)))
                            (mapcar (lambda (row) (entry-at self row)) (selected-rows self)))))
    (with-undo (self "Remove Description" entries)
      (dolist (entry entries)
        (handler-case
            (progn (remove-description (entry-path entry) :tags (if (tags-p self) '() :keep))
                   (setf (entry-saved entry) nil (entry-description entry) nil
                         (entry-edited entry) nil (entry-analysis entry) nil
                         (entry-status entry) nil (entry-message entry) nil)
                   (push entry removed)
                   (note "removed: ~a" (entry-path entry)))
          (error (condition)
            (incf failed)
            (setf (entry-message entry) (condition-text condition))))))
    (setf (controller-message self)
          (and (plusp failed) (format nil "~d description~:p could not be removed." failed)))
    (tell-finder self (nreverse removed))
    (objc:invoke (controller-table self) "reloadData")
    (show-selection self)
    (refresh-controls self)))

(define-controller-method ("revertDescription:" :void) ((sender objc:objc-object-pointer))
  (let ((entry (entry-at self (controller-editor-row self))))
    (when (and entry (not (eq (entry-state entry) :identifying)))
      (commit-editing self)
      (setf (entry-edited entry) nil
            (entry-description entry) (entry-saved entry))
      ;; With nothing written, back to what Vision proposed.
      (unless (entry-saved entry)
        (propose self entry))
      (objc:invoke (controller-table self) "reloadData")
      (show-selection self)
      (refresh-controls self))))

(define-controller-method ("showInFinder:" :void) ((sender objc:objc-object-pointer))
  (let* ((clicked (objc:invoke (controller-table self) "clickedRow"))
         (rows (if (>= clicked 0) (list clicked) (selected-rows self)))
         (paths (loop for row in rows
                      for entry = (entry-at self row)
                      when entry collect (objc:invoke "NSURL" "fileURLWithPath:"
                                                      (entry-path entry)))))
    (when paths
      (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace")
                   "activateFileViewerSelectingURLs:" (coerce paths 'vector)))))

;;; The selected image: its preview and its description --------------------------------

(defun editing-p (controller)
  "Whether the description field is being typed in."
  (not (null-object-p (objc:invoke (controller-editor controller) "currentEditor"))))

(defun show-preview (controller entry)
  "Show ENTRY's picture: at once if it was made before, else once the
preview thread has made it (previewReady:)."
  (let ((path (and entry (entry-path entry)))
        (view (controller-preview controller)))
    (unless (equal path (controller-preview-path controller))
      (setf (controller-preview-path controller) path)
      (if (or (null path) (null (controller-previewer controller)))
          (objc:invoke view "setImage:" nil)
          (multiple-value-bind (image found) (request-preview (controller-previewer controller) path)
            (objc:invoke view "setImage:" (and found image)))))))

(define-controller-method ("previewReady:" :void) ((argument objc:objc-object-pointer))
  (let ((path (controller-preview-path self)))
    (when (and path (controller-previewer self))
      (multiple-value-bind (image found) (cached-preview (controller-previewer self) path)
        (when found
          (objc:invoke (controller-preview self) "setImage:" image))))))

(defun placeholder-for (entry)
  (ecase (entry-state entry)
    ((nil) "Not identified yet. Press Identify, or type a description.")
    (:identifying "Identifying…")
    (:nothing "Vision found nothing it was sure of. Type a description, or lower the minimum confidence.")
    (:error (format nil "~@[~a ~]Type a description, or choose Identify Again."
                    (entry-message entry)))
    ((:proposed :edited :saved) "No description: accepting removes the one on the file.")))

(defun show-selection (controller)
  "Show the selected image and its description below the list."
  (let* ((rows (selected-rows controller))
         (editor (controller-editor controller))
         (entry (and (= 1 (length rows)) (entry-at controller (first rows)))))
    (show-preview controller (and rows (entry-at controller (first rows))))
    (cond (entry
           (objc:invoke (controller-title-label controller) "setStringValue:"
                        (format nil "~a — ~a" (entry-relative entry)
                                (let ((state (entry-state entry)))
                                  (if state (state-text state) "not identified"))))
           (let ((usable (not (eq (entry-state entry) :identifying))))
             (objc:invoke editor "setEditable:" usable)
             (objc:invoke editor "setEnabled:" usable))
           (objc:invoke editor "setPlaceholderString:" (placeholder-for entry))
           ;; Not while it is being typed in: that would move the insertion point.
           (unless (and (editing-p controller) (eql (controller-editor-row controller) (first rows)))
             (objc:invoke editor "setStringValue:" (or (entry-description entry) "")))
           (setf (controller-editor-row controller) (first rows)))
          (t
           (when (editing-p controller) (commit-editing controller))
           (setf (controller-editor-row controller) nil)
           (objc:invoke (controller-title-label controller) "setStringValue:"
                        (if rows (format nil "~d images selected" (length rows)) ""))
           (objc:invoke editor "setStringValue:" "")
           (objc:invoke editor "setPlaceholderString:"
                        (if rows
                            "Accept writes each selected image's own description."
                            "Select an image to see what was found in it."))
           (objc:invoke editor "setEditable:" nil)
           (objc:invoke editor "setEnabled:" nil)))))

(defun take-edit (controller)
  "Put what is in the description field into the row it is editing."
  (let ((entry (entry-at controller (controller-editor-row controller))))
    (when (and entry (not (eq (entry-state entry) :identifying)))
      (let ((text (field-string (controller-editor controller))))
        (unless (equal text (or (entry-description entry) ""))
          (setf (entry-description entry) text
                (entry-edited entry) t)
          (objc:invoke (controller-table controller) "reloadData")
          (refresh-controls controller))))))

;;; As each key is typed: the row shows it, and Accept can be pressed.
(define-controller-method ("controlTextDidChange:" :void) ((notification objc:objc-object-pointer))
  (take-edit self))

;;; Return in the description field.
(define-controller-method ("descriptionEntered:" :void) ((sender objc:objc-object-pointer))
  (take-edit self)
  (accept-selected self)
  ;; Straight on to typing in the next.
  (objc:invoke (controller-window self) "makeFirstResponder:" (controller-editor self)))

(define-controller-method ("tableViewSelectionDidChange:" :void)
    ((notification objc:objc-object-pointer))
  (setf (controller-message self) nil)
  (show-selection self)
  (refresh-controls self))

;;; The table's data ------------------------------------------------------------------

(defun column-key (column)
  (if (null-object-p column)
      ""
      (or (objc:invoke-into 'string column "identifier") "")))

(defun cell-text (controller key row)
  (let ((entry (entry-at controller row)))
    (cond ((null entry) "")
          ((string= key "status") (state-text (entry-state entry)))
          ((string= key "description")
           (substitute #\Space #\Newline (or (entry-description entry) "")))
          (t (entry-relative entry)))))

(define-controller-method ("numberOfRowsInTableView:" :long :on-error 0)
    ((table objc:objc-object-pointer))
  (length (controller-entries self)))

(define-controller-method ("tableView:objectValueForTableColumn:row:"
                           objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  ;; Autoreleased: an object a Lisp method answers is the caller's to release,
  ;; and a table releases nothing it is given here.
  (objc:string-to-ns-string (cell-text self (column-key column) row) t))

(defun emphasized-selection-p (table row)
  "Whether ROW is drawn in the accent colour: selected, in a table that has the
focus.  Without the focus a selection is pale grey, and wants ordinary text."
  (and (objc:invoke-bool table "isRowSelected:" row)
       (let ((window (objc:invoke table "window")))
         (and (not (null-object-p window))
              (objc:invoke-bool window "isKeyWindow")
              (cffi:pointer-eq (objc:invoke window "firstResponder") table)))))

(defun state-color (state)
  (case state
    (:saved "systemGreenColor")
    (:proposed "systemBlueColor")
    (:edited "systemOrangeColor")
    (:error "systemRedColor")
    ((:identifying :nothing) "secondaryLabelColor")
    (t "labelColor")))

;;; The status in colour.  A row selected in the accent colour keeps the
;;; selection's own text colour, or blue on blue would be unreadable.
(define-controller-method ("tableView:willDisplayCell:forTableColumn:row:" :void)
    ((table objc:objc-object-pointer)
     (cell objc:objc-object-pointer)
     (column objc:objc-object-pointer)
     (row :long))
  (let ((entry (entry-at self row))
        (key (column-key column)))
    (objc:invoke cell "setTextColor:"
                 (objc:invoke "NSColor"
                              (cond ((emphasized-selection-p table row)
                                     "alternateSelectedControlTextColor")
                                    ((and entry (string= key "status"))
                                     (state-color (entry-state entry)))
                                    ((and entry (string= key "description")
                                          (member (entry-state entry) '(:proposed)))
                                     "secondaryLabelColor")
                                    (t "labelColor"))))))

;;; Resting the pointer on a row shows all of its description, or what went wrong.
(define-controller-method ("tableView:toolTipForCell:rect:tableColumn:row:mouseLocation:"
                           objc:objc-object-pointer
                           :on-error (cffi:null-pointer))
    ((table objc:objc-object-pointer)
     (cell objc:objc-object-pointer)
     (rect (:pointer :void))
     (column objc:objc-object-pointer)
     (row :long)
     (location cocoa:ns-point))
  (let ((entry (entry-at self row)))
    (if entry
        (objc:string-to-ns-string
         (or (entry-message entry)
             (and (not (blank-p (entry-description entry))) (entry-description entry))
             (entry-path entry))
         t)
        (cffi:null-pointer))))

;;; Closing ---------------------------------------------------------------------------

(defun confirm-discard (controller)
  "Whether it is all right to lose the descriptions not yet accepted: there
are none, or the user says so."
  (let ((review (length (review-rows controller))))
    (or (controller-confirmed-quit controller)
        (zerop review)
        (let ((alert (objc:invoke (objc:invoke "NSAlert" "alloc") "init")))
          (unwind-protect
               (progn
                 (objc:invoke alert "setMessageText:"
                              (format nil "~d description~:p ~:*~[~;has~:;have~] not been accepted."
                                      review))
                 (objc:invoke alert "setInformativeText:"
                              "They are not written onto the images until they are accepted.")
                 (objc:invoke alert "addButtonWithTitle:" "Accept All and Quit")
                 (objc:invoke alert "addButtonWithTitle:" "Cancel")
                 (objc:invoke alert "addButtonWithTitle:" "Quit Without Accepting")
                 (let ((answer (objc:invoke alert "runModal")))
                   (case (- answer +ns-alert-first-button-return+)
                     (0 (accept-rows controller (review-rows controller))
                        (setf (controller-confirmed-quit controller) t))
                     (2 (setf (controller-confirmed-quit controller) t))
                     (t nil))))
            (objc:release alert))))))

(define-controller-method ("windowShouldClose:" objc:objc-bool :on-error t)
    ((window objc:objc-object-pointer))
  (commit-editing self)
  (confirm-discard self))

;;; Building it -----------------------------------------------------------------------

(defun add-label (content text frame mask &key alignment bold)
  (let ((label (objc:invoke "NSTextField" "labelWithString:" text)))
    (objc:invoke label "setFrame:" frame)
    (objc:invoke label "setAutoresizingMask:" mask)
    (objc:invoke (objc:invoke label "cell") "setLineBreakMode:" +ns-line-break-by-truncating-middle+)
    (when alignment
      ;; NSTextAlignmentRight is 2 on Apple silicon and Intel alike.
      (objc:invoke label "setAlignment:" alignment))
    (when bold
      (objc:invoke label "setFont:" (objc:invoke "NSFont" "boldSystemFontOfSize:" 13d0)))
    (objc:invoke content "addSubview:" label)
    label))

(defun add-path-field (content frame mask target placeholder)
  "A text field for a folder's path, which tells TARGET when it is changed."
  (let ((field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:" frame)))
    (objc:invoke field "setAutoresizingMask:" mask)
    (objc:invoke field "setPlaceholderString:" placeholder)
    (objc:invoke (objc:invoke field "cell") "setLineBreakMode:" +ns-line-break-by-truncating-middle+)
    (objc:invoke (objc:invoke field "cell") "setSendsActionOnEndEditing:" t)
    (objc:invoke field "setTarget:" target)
    (objc:invoke field "setAction:" (objc:coerce-to-selector "optionsChanged:"))
    (objc:invoke content "addSubview:" field)
    (objc:release field)
    field))

(defun add-editor (content frame mask target)
  "The description field: several lines, wrapped.  Each key typed tells TARGET
(controlTextDidChange:), and Return accepts (descriptionEntered:)."
  (let ((field (objc:invoke (objc:invoke "NSTextField" "alloc") "initWithFrame:" frame)))
    (objc:invoke field "setAutoresizingMask:" mask)
    (objc:invoke field "setUsesSingleLineMode:" nil)
    (let ((cell (objc:invoke field "cell")))
      (objc:invoke cell "setWraps:" t)
      (objc:invoke cell "setScrollable:" nil)
      (objc:invoke cell "setLineBreakMode:" +ns-line-break-by-word-wrapping+)
      (objc:invoke cell "setSendsActionOnEndEditing:" nil))
    (objc:invoke field "setFont:" (objc:invoke "NSFont" "systemFontOfSize:" 14d0))
    (objc:invoke field "setDelegate:" target)
    (objc:invoke field "setTarget:" target)
    (objc:invoke field "setAction:" (objc:coerce-to-selector "descriptionEntered:"))
    (objc:invoke content "addSubview:" field)
    (objc:release field)
    field))

(defun add-button (content title action frame mask target)
  (let ((button (objc:invoke "NSButton" "buttonWithTitle:target:action:"
                             title target (objc:coerce-to-selector action))))
    (objc:invoke button "setFrame:" frame)
    (objc:invoke button "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" button)
    button))

(defun add-checkbox (content title frame mask target action)
  (let ((checkbox (objc:invoke "NSButton" "checkboxWithTitle:target:action:"
                               title target (objc:coerce-to-selector action))))
    (objc:invoke checkbox "setFrame:" frame)
    (objc:invoke checkbox "setAutoresizingMask:" mask)
    (objc:invoke content "addSubview:" checkbox)
    checkbox))

(defun add-slider (content frame mask target)
  "The minimum confidence, 5 to 95 per cent.  Continuous, so the proposals
follow the knob."
  (let ((slider (objc:invoke "NSSlider" "sliderWithValue:minValue:maxValue:target:action:"
                             30d0 5d0 95d0 target (objc:coerce-to-selector "settingsChanged:"))))
    (objc:invoke slider "setFrame:" frame)
    (objc:invoke slider "setAutoresizingMask:" mask)
    (objc:invoke slider "setContinuous:" t)
    (objc:invoke content "addSubview:" slider)
    slider))

(defun add-preview (content frame mask)
  (let ((view (objc:invoke (objc:invoke "NSImageView" "alloc") "initWithFrame:" frame)))
    (objc:invoke view "setAutoresizingMask:" mask)
    (objc:invoke view "setImageFrameStyle:" +ns-image-frame-gray-bezel+)
    (objc:invoke view "setImageScaling:" +ns-image-scale-proportionally-down+)
    (objc:invoke view "setEditable:" nil)
    (objc:invoke content "addSubview:" view)
    (objc:release view)
    view))

(defun add-column (table key title width &key (resizing 2))
  (let ((column (objc:invoke (objc:invoke "NSTableColumn" "alloc") "initWithIdentifier:" key)))
    (objc:invoke column "setTitle:" title)
    (objc:invoke column "setWidth:" width)
    (objc:invoke column "setResizingMask:" resizing)
    (objc:invoke column "setEditable:" nil)
    (objc:invoke (objc:invoke column "dataCell") "setLineBreakMode:"
                 (if (string= key "description")
                     +ns-line-break-by-truncating-tail+
                     +ns-line-break-by-truncating-middle+))
    (objc:invoke table "addTableColumn:" column)
    (objc:release column)
    column))

(defun add-table (content frame mask target)
  "The list of images in its scroll view.  Answers the table."
  (let ((scroll (objc:invoke (objc:invoke "NSScrollView" "alloc") "initWithFrame:" frame))
        ;; At the scroll view's own origin: FRAME's is a place in the window,
        ;; and a table given it draws its rows that far from its headers.
        (table (objc:invoke (objc:invoke "NSTableView" "alloc") "initWithFrame:"
                            (vector 0d0 0d0 (aref frame 2) (aref frame 3)))))
    (objc:invoke table "setStyle:" +ns-table-view-style-full-width+)
    (add-column table "file" "Image" 240d0 :resizing 3)
    (add-column table "status" "Status" 96d0)
    ;; The description takes whatever width is going.
    (add-column table "description" "Description" (- (aref frame 2) 240d0 96d0 30d0) :resizing 3)
    (objc:invoke table "setUsesAlternatingRowBackgroundColors:" t)
    (objc:invoke table "setAllowsMultipleSelection:" t)
    (objc:invoke table "setColumnAutoresizingStyle:"
                 +ns-table-view-last-column-only-autoresizing-style+)
    (objc:invoke table "setDataSource:" target)
    (objc:invoke table "setDelegate:" target)
    (objc:invoke table "setTarget:" target)
    (objc:invoke table "setDoubleAction:" (objc:coerce-to-selector "showInFinder:"))
    (objc:invoke scroll "setHasVerticalScroller:" t)
    (objc:invoke scroll "setAutohidesScrollers:" t)
    (objc:invoke scroll "setBorderType:" +ns-bezel-border+)
    (objc:invoke scroll "setAutoresizingMask:" mask)
    (objc:invoke scroll "setDocumentView:" table)
    (objc:invoke content "addSubview:" scroll)
    (objc:release table)
    (objc:release scroll)
    table))

(defparameter +window-width+ 880d0)
(defparameter +window-height+ 760d0)
(defparameter +options-height+ 152d0)
(defparameter +detail-height+ 190d0)
(defparameter +bottom-bar+ 56d0)

(defun add-options (controller content target)
  "The options, in a box across the top of the window: the folder, what to
find in each image, and what to write besides the comment and keywords."
  (let ((box (objc:invoke (objc:invoke "NSBox" "alloc") "initWithFrame:"
                          (vector 14d0 (- +window-height+ +options-height+ 10d0)
                                  (- +window-width+ 28d0) +options-height+))))
    (objc:invoke box "setTitle:" "Options")
    (objc:invoke box "setAutoresizingMask:" (logior +flexible-width+ +flexible-bottom+))
    (objc:invoke content "addSubview:" box)
    (objc:release box)
    (let* ((inner (objc:invoke box "contentView"))
           (size (objc:invoke inner "frame"))
           (width (aref size 2))
           (height (aref size 3))
           (wide (logior +flexible-width+ +flexible-bottom+))
           (right (logior +flexible-left+ +flexible-bottom+))
           (left +flexible-bottom+)
           (x 100d0)
           (y1 (- height 32d0))
           (y2 (- height 62d0))
           (y3 (- height 90d0))
           (y4 (- height 118d0)))
      (flet ((checkbox (title x y w action)
               (add-checkbox inner title (vector x (+ y 2d0) w 18d0) left target action)))
        (add-label inner "Folder:" (vector 8d0 (+ y1 3d0) 88d0 17d0) left :alignment 2)
        (setf (controller-folder-field controller)
              (add-path-field inner (vector x y1 (- width x 112d0) 22d0) wide target
                              "The folder of images to describe — or drop images or folders on the list"))
        (let ((choose (add-button inner "Choose…" "chooseFolder:"
                                  (vector (- width 106d0) (- y1 6d0) 100d0 32d0) right target)))
          (setf (controller-recursive-checkbox controller)
                (checkbox "Include subfolders" x y2 150d0 "optionsChanged:"))
          (setf (controller-option-controls controller)
                (list (controller-folder-field controller) choose
                      (controller-recursive-checkbox controller))))
        (add-label inner "Find:" (vector 8d0 (+ y2 3d0) 88d0 17d0) left :alignment 2)
        (setf (controller-text-checkbox controller)
              (checkbox "Text in images" 262d0 y2 130d0 "settingsChanged:"))
        (add-label inner "Minimum confidence:" (vector (- width 400d0) (+ y2 3d0) 136d0 17d0)
                   right :alignment 2)
        (setf (controller-confidence-slider controller)
              (add-slider inner (vector (- width 258d0) y2 200d0 22d0) right target)
              (controller-confidence-label controller)
              (add-label inner "" (vector (- width 52d0) (+ y2 3d0) 44d0 17d0) right))
        (setf (controller-places-checkbox controller)
              (checkbox "Place names (asks Apple)" x y3 200d0 "settingsChanged:"))
        (add-label inner "Also:" (vector 8d0 (+ y4 3d0) 88d0 17d0) left :alignment 2)
        (setf (controller-tags-checkbox controller)
              (checkbox "Tag each in Finder with its first keyword" x y4 290d0 "settingsChanged:")
              (controller-get-info-checkbox controller)
              (checkbox "Show in Get Info (asks Finder)" 400d0 y4 260d0 "settingsChanged:"))))))

(defun add-detail (controller content target)
  "Below the list: the selected image, and its description to edit."
  (let* ((bottom +bottom-bar+)
         (top (+ bottom +detail-height+))
         (fixed +flexible-top+)
         (x 224d0)
         (width (- +window-width+ x 20d0))
         (corner (logior +flexible-left+ +flexible-top+)))
    (setf (controller-preview controller)
          (add-preview content (vector 20d0 bottom 190d0 +detail-height+) fixed)
          (controller-title-label controller)
          (add-label content "" (vector x (- top 18d0) width 17d0)
                     (logior +flexible-width+ +flexible-top+) :bold t)
          (controller-editor controller)
          (add-editor content (vector x (+ bottom 40d0) width (- +detail-height+ 66d0))
                      (logior +flexible-width+ +flexible-top+) target))
    (add-label content "Return accepts and goes on to the next."
               (vector x (+ bottom 10d0) (- width 360d0) 17d0)
               (logior +flexible-width+ +flexible-top+))
    (let ((right (- +window-width+ 20d0)))
      (setf (controller-revert-button controller)
            (add-button content "Revert" "revertDescription:"
                        (vector (- right 104d0 136d0 96d0) (+ bottom 2d0) 90d0 32d0) corner target)
            (controller-again-button controller)
            (add-button content "Identify Again" "identifyAgain:"
                        (vector (- right 104d0 136d0) (+ bottom 2d0) 130d0 32d0) corner target)
            (controller-accept-button controller)
            (add-button content "Accept" "acceptSelected:"
                        (vector (- right 104d0) (+ bottom 2d0) 104d0 32d0) corner target)))))

(defun restore-settings (controller)
  "Put the remembered options into the controls."
  (let ((folder (default-string "Folder")))
    (when folder
      (objc:invoke (controller-folder-field controller) "setStringValue:" (abbreviate-path folder)))
    (set-checked (controller-recursive-checkbox controller) (default-bool "IncludeSubfolders" nil))
    (set-checked (controller-text-checkbox controller) (default-bool "ReadText" t))
    (set-checked (controller-places-checkbox controller) (default-bool "PlaceNames" nil))
    (set-checked (controller-tags-checkbox controller) (default-bool "FinderTags" nil))
    (set-checked (controller-get-info-checkbox controller) (default-bool "GetInfoComments" nil))
    (objc:invoke (controller-confidence-slider controller) "setIntegerValue:"
                 (max 5 (min 95 (default-integer "MinimumConfidence"
                                                 (round (* 100 *default-minimum-confidence*))))))
    (show-confidence controller)))

(defun make-window-controller (&key (previews t))
  "A controller with its window built and not yet shown.  With PREVIEWS, a
thread to make the selected image's picture."
  (let* ((controller (make-instance 'window-controller))
         (target (objc:objc-object-pointer controller))
         (window (objc:invoke (objc:invoke "NSWindow" "alloc")
                              "initWithContentRect:styleMask:backing:defer:"
                              (vector 0d0 0d0 +window-width+ +window-height+)
                              +ns-window-style-mask+ +ns-backing-store-buffered+ t))
         (content (objc:invoke window "contentView")))
    ;; Or the close button frees a window this still points at.
    (objc:invoke window "setReleasedWhenClosed:" nil)
    (objc:invoke window "setTitle:" "Image Describer")
    (objc:invoke window "setContentMinSize:" #(760d0 620d0))
    ;; So that menu items with no target -- File > Identify -- reach it.
    (objc:invoke window "setDelegate:" target)
    (setf (controller-window controller) window
          (controller-undo-manager controller) (objc:alloc-init-object "NSUndoManager"))
    (when previews
      (setf (controller-previewer controller)
            (make-previewer (lambda (path)
                              (declare (ignore path))
                              (objc:invoke target "performSelectorOnMainThread:withObject:waitUntilDone:"
                                           (objc:coerce-to-selector "previewReady:") nil nil)))))
    (add-options controller content target)
    ;; From the bottom up: the status line and the buttons, the selected
    ;; image, then the list.
    (setf (controller-status-label controller)
          (add-label content "" (vector 20d0 22d0 (- +window-width+ 290d0) 17d0)
                     (logior +flexible-width+ +flexible-top+))
          (controller-accept-all-button controller)
          (add-button content "Accept All" "acceptAll:"
                      (vector (- +window-width+ 254d0) 14d0 120d0 32d0)
                      (logior +flexible-left+ +flexible-top+) target)
          (controller-identify-button controller)
          (add-button content "Identify" "identify:"
                      (vector (- +window-width+ 130d0) 14d0 116d0 32d0)
                      (logior +flexible-left+ +flexible-top+) target))
    (add-detail controller content target)
    (let ((top (- +window-height+ +options-height+ 18d0))
          (bottom (+ +bottom-bar+ +detail-height+ 12d0)))
      (setf (controller-table controller)
            (add-table content (vector 20d0 bottom (- +window-width+ 40d0) (- top bottom))
                       (logior +flexible-width+ +flexible-height+) target)))
    ;; Images and folders dropped on the list.
    (objc:invoke (controller-table controller) "registerForDraggedTypes:"
                 (objc:invoke "NSArray" "arrayWithObject:" (ns-string "public.file-url")))
    (restore-settings controller)
    (objc:invoke window "center")
    (rescan controller)
    controller))

(defun close-window-controller (controller)
  "Stop what CONTROLLER runs and close its window."
  (stop-and-wait controller)
  (setf (controller-run controller) nil)
  (when (controller-previewer controller)
    (stop-previewer (controller-previewer controller))
    (setf (controller-previewer controller) nil))
  (objc:invoke (controller-window controller) "close"))

;;; Dropping images and folders ------------------------------------------------------

(defun paths-from-pasteboard (pasteboard)
  "The paths of the files on PASTEBOARD."
  (let ((urls (objc:invoke pasteboard "readObjectsForClasses:options:"
                           (objc:invoke "NSArray" "arrayWithObject:" (objc:invoke "NSURL" "class"))
                           (dictionary "NSPasteboardURLReadingFileURLsOnlyKey" t))))
    (if (null-object-p urls)
        '()
        (loop for i below (objc:invoke urls "count")
              collect (objc:invoke-into 'string (objc:invoke urls "objectAtIndex:" i) "path")))))

(defun common-folder (paths)
  "The deepest folder holding every one of PATHS."
  (let ((parts (mapcar (lambda (path) (butlast (uiop:split-string path :separator "/"))) paths)))
    (let ((common (first parts)))
      (dolist (other (rest parts))
        (setf common (loop for a in common for b in other while (string= a b) collect a)))
      (let ((joined (format nil "~{~a~^/~}" common)))
        (if (zerop (length joined)) "/" joined)))))

(defun open-paths (controller paths)
  "Show PATHS: a folder is chosen as the folder; images are shown in theirs,
with subfolders if they are in several, and selected."
  (let* ((paths (mapcar (lambda (path) (string-right-trim "/" path)) paths))
         (folder (find-if #'directory-exists-p paths))
         (images (remove-if-not (lambda (path) (and (not (directory-exists-p path)) (image-file-p path)))
                                paths)))
    (cond ((controller-running controller)
           (setf (controller-message controller) "Stop identifying first.")
           (refresh-controls controller)
           nil)
          (folder
           (set-folder controller folder)
           t)
          (images
           (let* ((canonical (mapcar (lambda (path)
                                       (native (truename (sb-ext:parse-native-namestring path))))
                                     images))
                  (common (common-folder canonical))
                  (several (some (lambda (path)
                                   (string/= common (subseq path 0 (position #\/ path :from-end t))))
                                 canonical)))
             (objc:invoke (controller-folder-field controller) "setStringValue:"
                          (abbreviate-path common))
             (when several
               (set-checked (controller-recursive-checkbox controller) t))
             (rescan controller :select canonical)
             (save-settings controller)
             t))
          (t
           (setf (controller-message controller) "Nothing dropped is an image or a folder.")
           (refresh-controls controller)
           nil))))

(define-controller-method ("tableView:validateDrop:proposedRow:proposedDropOperation:"
                           (:unsigned :long) :on-error 0)
    ((table objc:objc-object-pointer)
     (info objc:objc-object-pointer)
     (row :long)
     (operation (:unsigned :long)))
  (cond ((controller-running self) 0)
        (t
         ;; Onto the whole list, not between two rows.
         (objc:invoke table "setDropRow:dropOperation:" -1 0)
         1)))                           ; NSDragOperationCopy

(define-controller-method ("tableView:acceptDrop:row:dropOperation:" objc:objc-bool :on-error nil)
    ((table objc:objc-object-pointer)
     (info objc:objc-object-pointer)
     (row :long)
     (operation (:unsigned :long)))
  (open-paths self (paths-from-pasteboard (objc:invoke info "draggingPasteboard"))))

(defun show-window (controller)
  (objc:invoke (controller-window controller) "makeKeyAndOrderFront:" (cffi:null-pointer))
  (objc:invoke (objc.runloop:shared-application) "activateIgnoringOtherApps:" t)
  controller)
