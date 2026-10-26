;;;; preview.lisp -- the selected image's picture, made off the main thread.
;;;;
;;;; Selecting a 6000-pixel photograph used to decode it on the main thread,
;;;; and the window stopped while it did.  Now a thread of its own makes a
;;;; thumbnail -- from ImageIO, which uses the one a camera stores in the file
;;;; when it can, else from IM -- and the main thread only shows it.  Only the
;;;; latest request is made: clicking down a list of fifty does not queue
;;;; fifty.  The last few are kept, so going back is immediate.
;;;;
;;;; An NSImage may be made on any thread; it is shown only on the main one.

(in-package #:image-describe)

(defparameter *preview-side* 600
  "The longest side of a preview thumbnail, in pixels.")

(defparameter *preview-cache-size* 48)

(defstruct (previewer (:constructor %make-previewer))
  (lock (sb-thread:make-mutex :name "preview"))
  (wake (sb-thread:make-waitqueue))
  (wanted nil)                          ; the path to make next
  (cache '())                           ; ((path . nsimage) ...), newest first
  (thread nil)
  (on-ready nil))                       ; called on the preview thread with a path

(defun nsimage-from-cg (cg)
  "A new NSImage of CG, which the caller releases."
  (objc:invoke (objc:invoke "NSImage" "alloc") "initWithCGImage:size:" cg #(0d0 0d0)))

(defun make-thumbnail (path)
  "A new NSImage of the image at PATH, no larger than *PREVIEW-SIDE*, or NIL."
  (let ((cg (or (image-io-thumbnail path *preview-side*)
                (ignore-errors
                 (multiple-value-bind (rgb orientation) (load-for-vision path :largest *preview-side*)
                   (declare (ignore orientation))
                   (unwind-protect (cg-image-from-rgb rgb) (im:destroy rgb)))))))
    (when cg
      (unwind-protect (nsimage-from-cg cg)
        (cf-release cg)))))

(defun cached-preview (previewer path)
  "The NSImage made for PATH, NIL if none could be, and whether one was
tried: it is then newest in the cache."
  (sb-thread:with-mutex ((previewer-lock previewer))
    (let ((cell (assoc path (previewer-cache previewer) :test #'equal)))
      (when cell
        (setf (previewer-cache previewer)
              (cons cell (remove cell (previewer-cache previewer))))
        (values (cdr cell) t)))))

(defun keep-preview (previewer path image)
  (sb-thread:with-mutex ((previewer-lock previewer))
    (push (cons path image) (previewer-cache previewer))
    (let ((extra (nthcdr *preview-cache-size* (previewer-cache previewer))))
      (when extra
        (dolist (cell extra) (when (cdr cell) (objc:release (cdr cell))))
        (setf (previewer-cache previewer)
              (ldiff (previewer-cache previewer) extra))))))

(defun preview-loop (previewer)
  (loop
    (let ((path (sb-thread:with-mutex ((previewer-lock previewer))
                  (loop until (previewer-wanted previewer)
                        do (sb-thread:condition-wait (previewer-wake previewer)
                                                     (previewer-lock previewer)))
                  (shiftf (previewer-wanted previewer) nil))))
      (when (eq path :quit) (return))
      (unless (nth-value 1 (cached-preview previewer path))
        (let ((image (ignore-errors (objc:with-autorelease-pool () (make-thumbnail path)))))
          (keep-preview previewer path image)))
      (ignore-errors (funcall (previewer-on-ready previewer) path)))))

(defun make-previewer (on-ready)
  "A previewer whose thread calls ON-READY with each path it has made a
picture of.  ON-READY runs on that thread and must not touch AppKit's views."
  (let ((previewer (%make-previewer :on-ready on-ready)))
    (setf (previewer-thread previewer)
          (sb-thread:make-thread #'preview-loop :name "preview" :arguments (list previewer)))
    previewer))

(defun request-preview (previewer path)
  "The picture of PATH and true if it is kept already (NIL and true if none
could be made); else NIL and NIL, and it is made, and ON-READY told."
  (multiple-value-bind (image found) (cached-preview previewer path)
    (if found
        (values image t)
        (progn
          (sb-thread:with-mutex ((previewer-lock previewer))
            (setf (previewer-wanted previewer) path)
            (sb-thread:condition-notify (previewer-wake previewer)))
          (values nil nil)))))

(defun stop-previewer (previewer)
  (sb-thread:with-mutex ((previewer-lock previewer))
    (setf (previewer-wanted previewer) :quit)
    (sb-thread:condition-notify (previewer-wake previewer)))
  (ignore-errors (sb-thread:join-thread (previewer-thread previewer) :timeout 5 :default nil)))
