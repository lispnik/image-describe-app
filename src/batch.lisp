;;;; batch.lisp -- the images, what was found in each, and the worker pool.
;;;;
;;;; This is what the window's table shows, one entry a row.  Nothing here
;;;; knows about the window, which is what lets the tests drive it directly.
;;;;
;;;; Who changes what, since the workers and the main thread share entries:
;;;; a worker sets an entry's STATUS, ANALYSIS and MESSAGE, and only while it
;;;; holds the entry; everything else -- the description, what was saved,
;;;; whether it was edited -- belongs to the main thread, which proposes a
;;;; description from the analysis once the worker is done with it.  So an
;;;; edit made while the batch runs is never written over by a worker.

(in-package #:image-describe)

(defstruct entry
  (path "" :type string)                ; native path of the image
  (relative "" :type string)            ; PATH relative to the folder chosen
  (status nil)                          ; NIL, :IDENTIFYING, :IDENTIFIED or :ERROR
  (analysis nil)                        ; what Vision found, an ANALYSIS
  (description nil)                     ; the text shown, accepted, written
  (saved nil)                           ; the description the file carries
  (edited nil)                          ; DESCRIPTION was typed, not proposed
  (message nil))                        ; why identifying or saving failed

(defun blank-p (text)
  (or (null text) (zerop (length (string-trim '(#\Space #\Tab #\Newline) text)))))

(defun entry-state (entry)
  "Where ENTRY is, as the Status column says it:
  NIL           nothing known yet
  :IDENTIFYING  a worker has it
  :PROPOSED     a description to review
  :EDITED       a description typed and not yet saved
  :SAVED        the file carries the description shown
  :NOTHING      Vision found nothing to say
  :ERROR        identifying or saving it failed"
  (let ((description (entry-description entry)))
    (cond ((eq (entry-status entry) :identifying) :identifying)
          ((eq (entry-status entry) :error) :error)
          ((and (not (blank-p description)) (equal description (entry-saved entry))) :saved)
          ((and (entry-edited entry) (not (equal description (entry-saved entry)))) :edited)
          ((not (blank-p description)) :proposed)
          ((eq (entry-status entry) :identified) :nothing)
          (t nil))))

(defun state-text (state)
  "The table's Status column."
  (ecase state
    ((nil) "")
    (:identifying "identifying…")
    (:proposed "to review")
    (:edited "edited")
    (:saved "saved")
    (:nothing "nothing found")
    (:error "error")))

(defun entry-for (path root)
  "An entry for the image at PATH, with the description it already carries."
  (let ((saved (read-description path)))
    (make-entry :path path :relative (relative-name path root)
                :saved saved :description saved)))

(defun entries-from-directory (directory &key recursive)
  "An entry for each image in DIRECTORY, with the description each file
already carries."
  (let ((root (canonical-path directory)))
    (map 'vector (lambda (path) (entry-for path root))
         (scan-directory directory :recursive recursive))))

(defun start-listing (directory &key recursive on-done)
  "List DIRECTORY's images as ENTRIES-FROM-DIRECTORY does, on a thread of its
own, and call ON-DONE there with the vector of entries, or with NIL and the
reason it could not.  Answers the thread.

A folder of fifty thousand images on a USB disk takes a while to list, and
each file's attributes are read; done on the main thread, that was a window
that did not answer."
  (sb-thread:make-thread
   (lambda ()
     (multiple-value-bind (entries condition)
         (ignore-errors (entries-from-directory directory :recursive recursive))
       (funcall on-done entries (and condition (condition-text condition)))))
   :name "list images"))

(defun begin-identifying (entry)
  "Mark ENTRY as in a worker's hands."
  (setf (entry-message entry) nil
        (entry-status entry) :identifying))

(defun identification (entry &key (read-text t))
  "Ask Vision what is in ENTRY's image, without changing ENTRY.  Answers a
function of no arguments that records the answer in ENTRY: its analysis, or
why not in its message.  Never signals.

The worker pool calls that function under its lock, and not at all if the run
was abandoned meanwhile (ABANDON-BATCH): an image on a slow disk can take
minutes, and by then the row is the main thread's again."
  (let ((analysis nil) (message nil))
    (handler-case (setf analysis (analyze-image (entry-path entry) :read-text read-text))
      (serious-condition (condition)
        (setf message (condition-text condition))))
    (lambda ()
      ;; The status last: the main thread may read ENTRY meanwhile, and an
      ;; :IDENTIFIED or :ERROR it sees comes with what goes with it.
      (setf (entry-analysis entry) analysis
            (entry-message entry) message
            (entry-status entry) (if message :error :identified)))))

(defun identify-entry (entry &key (read-text t))
  "Identify ENTRY's image and record the answer in ENTRY.  Never signals;
answers ENTRY."
  (begin-identifying entry)
  (funcall (identification entry :read-text read-text))
  entry)

(defun propose-description (entry &key (minimum-confidence *default-minimum-confidence*)
                                       (include-text t) (include-place t))
  "Make ENTRY's description the one its analysis proposes, unless the user
has typed one.  For the main thread.  Answers whether it changed."
  (when (and (entry-analysis entry)
             (eq (entry-status entry) :identified)
             (not (entry-edited entry)))
    (let ((proposed (compose-description (entry-analysis entry)
                                         :minimum-confidence minimum-confidence
                                         :include-text include-text
                                         :include-place include-place)))
      (unless (equal proposed (entry-description entry))
        (setf (entry-description entry) proposed)
        t))))

(defun accept-entry (entry &key (tags :keep))
  "Write ENTRY's description onto its file, or take it off if blank; TAGS as
WRITE-DESCRIPTION takes them.  For the main thread.  Answers true if it was
written; if not, the message says why."
  (handler-case
      (let ((written (write-description (entry-path entry) (entry-description entry)
                                        :tags tags)))
        (setf (entry-saved entry) (if (blank-p written) nil written)
              (entry-description entry) (entry-saved entry)
              (entry-edited entry) nil
              (entry-message entry) nil)
        (when (eq (entry-status entry) :error)
          (setf (entry-status entry) (and (entry-analysis entry) :identified)))
        t)
    (error (condition)
      (setf (entry-message entry) (condition-text condition)
            (entry-status entry) :error)
      nil)))

;;; Identifying a batch on several threads ------------------------------------------
;;;
;;; Each worker takes the next entry and identifies it; nothing else is shared
;;; but the run's own bookkeeping, under its lock.  The time goes to IM's
;;; decoding, in C, and to Vision, and SBCL threads run foreign calls in
;;; parallel.  Threads SBCL made, not Grand Central Dispatch's: a stock SBCL
;;; cannot stop GCD's threads for a garbage collection, and dies.
;;;
;;; The window learns of progress through ON-PROGRESS, called on the worker's
;;; thread after each image and when a worker ends; it must not touch AppKit
;;; there, only ask the main thread to look (TAKE-FINISHED).

(defun processor-count ()
  "How many processors macOS is running work on now."
  (or (ignore-errors
       (cffi:with-foreign-objects ((count :int) (size :size))
         (setf (cffi:mem-ref size :size) (cffi:foreign-type-size :int))
         (and (zerop (cffi:foreign-funcall "sysctlbyname" :string "hw.activecpu"
                                           :pointer count :pointer size
                                           :pointer (cffi:null-pointer) :size 0 :int))
              (plusp (cffi:mem-ref count :int))
              (cffi:mem-ref count :int))))
      1))

(defvar *worker-count* nil
  "How many images are identified at once: NIL for DEFAULT-WORKER-COUNT.")

(defun default-worker-count ()
  "One a processor, and no more than eight: past that Vision, which has the
Neural Engine and the GPU to itself, is the queue, and each worker holds a
decoded photograph."
  (max 1 (min 8 (processor-count))))

(defstruct (batch-run (:conc-name run-) (:constructor %make-batch-run))
  (entries #() :type vector)
  (indices #() :type vector)            ; the entries to do, in order
  function                              ; called with each entry
  (begin nil)                           ; called with each entry as it is handed out
  (next 0)                              ; the position in INDICES handed out next
  (live 0)                              ; workers not yet finished
  (stopping nil)                        ; no more are handed out
  (abandoned nil)                       ; what is in hand is not to be recorded
  (in-hand '())                         ; indices handed out and not finished
  (finished '())                        ; indices done since TAKE-FINISHED
  (lock (sb-thread:make-mutex :name "batch run"))
  (threads '())
  (on-progress nil))

(defun start-batch (entries function &key indices on-progress begin
                                          (workers (or *worker-count* (default-worker-count))))
  "Call FUNCTION on each of ENTRIES, a vector, on WORKERS threads; with
INDICES, a sequence of positions in ENTRIES, on those alone.  Answers the run
at once.

If FUNCTION answers a function, that is called, under the run's lock, to
record what it found -- unless the run was abandoned meanwhile.  BEGIN, if
given, is called with each entry as it is handed out, also under the lock.
ON-PROGRESS, if given, is called with the run on a worker's thread after each
entry and as each worker ends."
  (let* ((entries (coerce entries 'vector))
         (indices (coerce (or indices (loop for i below (length entries) collect i)) 'vector))
         (run (%make-batch-run :entries entries :indices indices :function function
                               :begin begin :on-progress on-progress))
         (count (max 1 (min workers (length indices)))))
    (setf (run-live run) (if (zerop (length indices)) 0 count))
    (when (plusp (length indices))
      (setf (run-threads run)
            (loop for i below count
                  collect (sb-thread:make-thread #'batch-worker
                                                 :name (format nil "identify ~d" i)
                                                 :arguments (list run)))))
    run))

(defun next-index (run)
  "The index of the next entry, or NIL when there are none or the run is stopping."
  (sb-thread:with-mutex ((run-lock run))
    (let ((next (run-next run)))
      (unless (or (run-stopping run) (>= next (length (run-indices run))))
        (incf (run-next run))
        (let ((index (aref (run-indices run) next)))
          (push index (run-in-hand run))
          (when (run-begin run)
            (funcall (run-begin run) (aref (run-entries run) index)))
          index)))))

(defun notify (run)
  (let ((function (run-on-progress run)))
    (when function
      (ignore-errors (funcall function run)))))

(defun batch-worker (run)
  (unwind-protect
       (loop for index = (next-index run)
             while index
             do (let* ((entry (aref (run-entries run) index))
                       ;; The function handles every error an image can cause;
                       ;; this is for one it did not foresee, which must still
                       ;; leave the row answered and the worker going.
                       (record (handler-case (funcall (run-function run) entry)
                                 (serious-condition (condition)
                                   (let ((message (condition-text condition)))
                                     (lambda ()
                                       (setf (entry-message entry) message
                                             (entry-status entry) :error)))))))
                  (sb-thread:with-mutex ((run-lock run))
                    (setf (run-in-hand run) (remove index (run-in-hand run)))
                    (unless (run-abandoned run)
                      (when (functionp record)
                        (funcall record))
                      (push index (run-finished run))))
                  (notify run)))
    (sb-thread:with-mutex ((run-lock run))
      (decf (run-live run)))
    (notify run)))

(defun stop-batch (run)
  "Hand out no more entries.  Those in hand are finished."
  (sb-thread:with-mutex ((run-lock run))
    (setf (run-stopping run) t)))

(defun abandon-batch (run)
  "Hand out no more entries, and record nothing more: those in hand are
left to finish on their own, and what they find is thrown away.  Answers the
indices of the entries that were in hand, whose rows are the caller's again."
  (prog1 (sb-thread:with-mutex ((run-lock run))
           (setf (run-stopping run) t
                 (run-abandoned run) t)
           (prog1 (run-in-hand run)
             (setf (run-in-hand run) '())))
    ;; Vision stops at once when asked; IM's reading cannot be stopped.
    (cancel-requests (run-threads run))))

(defun batch-done-p (run)
  "Whether every worker has ended, or the run was abandoned."
  (sb-thread:with-mutex ((run-lock run))
    (or (run-abandoned run) (zerop (run-live run)))))

(defun take-finished (run)
  "The indices of the entries finished since this was last asked, oldest first."
  (sb-thread:with-mutex ((run-lock run))
    (prog1 (nreverse (run-finished run))
      (setf (run-finished run) '()))))

(defun wait-for-batch (run &key timeout)
  "Wait for every worker to end, or TIMEOUT seconds.  True if they all did."
  (let ((deadline (and timeout (+ (get-internal-real-time)
                                  (* timeout internal-time-units-per-second)))))
    (dolist (thread (run-threads run) t)
      (let ((left (and deadline (/ (- deadline (get-internal-real-time))
                                   internal-time-units-per-second))))
        (when (and left (<= left 0))
          (return nil))
        (multiple-value-bind (result outcome)
            (sb-thread:join-thread thread :default nil :timeout left)
          (declare (ignore result))
          (when (eq outcome :timeout)
            (return nil)))))))
