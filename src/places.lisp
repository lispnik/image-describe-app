;;;; places.lisp -- where a photograph was taken, in words.
;;;;
;;;; A photograph's GPS position is a pair of numbers; CLGeocoder turns it
;;;; into "Yosemite National Park, CA, United States".  That asks Apple's
;;;; servers, so it is done only when chosen.  Apple limits how often one
;;;; program may ask, so positions are looked up one at a time, a little apart,
;;;; and each answer is kept for every photograph taken within about a hundred
;;;; metres of it.
;;;;
;;;; All of this is for the main thread: CLGeocoder calls its completion
;;;; handler on the main queue, and the main thread is one SBCL made, so the
;;;; handler runs Lisp safely.  (On a libdispatch worker it would not.)

(in-package #:image-describe)

(defvar *geocoder* nil)
(defvar *places* (make-hash-table :test #'equal)
  "Place names already looked up, by rounded position.  NIL for none found.")
(defvar *place-queue* '()
  "Lookups waiting: (key latitude longitude callbacks...).")
(defvar *place-busy* nil)

(defparameter *place-interval* 0.6d0
  "Seconds between one lookup and the next.")

(defun forget-places ()
  (setf *geocoder* nil *place-queue* '() *place-busy* nil)
  (clrhash *places*))

(pushnew 'forget-places sb-ext:*save-hooks*)

(defun place-key (latitude longitude)
  (list (round (* latitude 1000)) (round (* longitude 1000))))

(defun placemark-name (placemark)
  "\"Yosemite National Park, CA, United States\": the area of interest if
there is one, the town, the region and the country, each once."
  (flet ((field (name)
           (let ((value (objc:invoke placemark name)))
             (and (not (null-object-p value)) (objc:ns-string-to-string value)))))
    (let* ((areas (objc:invoke placemark "areasOfInterest"))
           (area (and (not (null-object-p areas)) (plusp (objc:invoke areas "count"))
                      (objc:ns-string-to-string (objc:invoke areas "objectAtIndex:" 0))))
           (parts (remove-duplicates
                   (remove-if (lambda (part) (or (null part) (zerop (length part))))
                              (list area (field "locality") (field "administrativeArea")
                                    (field "country")))
                   :test #'string-equal :from-end t)))
      (and parts (format nil "~{~a~^, ~}" parts)))))

(defun lookup-place (latitude longitude callback)
  "Call CALLBACK, on the main thread, with the name of the place at LATITUDE
and LONGITUDE, or NIL if none is found.  At once if it has been looked up."
  (let ((key (place-key latitude longitude)))
    (multiple-value-bind (name found) (gethash key *places*)
      (if found
          (funcall callback name)
          (let ((waiting (assoc key *place-queue* :test #'equal)))
            (if waiting
                (nconc waiting (list callback))
                (setf *place-queue* (append *place-queue*
                                            (list (list key latitude longitude callback)))))
            (unless *place-busy*
              (next-place)))))))

(defun finish-place (key name callbacks)
  (setf (gethash key *places*) name)
  (dolist (callback callbacks)
    (ignore-errors (funcall callback name))))

(defun after-delay (seconds function)
  "Call FUNCTION on the main thread after SECONDS, from the run loop."
  (objc:with-objc-block (block '(:void (objc:objc-object-pointer))
                          (lambda (timer)
                            (declare (ignore timer))
                            (handler-case (funcall function)
                              (error (condition) (note "after-delay: ~a" condition)))))
    (objc:invoke "NSTimer" "scheduledTimerWithTimeInterval:repeats:block:"
                 (float seconds 1d0) nil block)))

(defun next-place ()
  "Look up the first position waiting, if any."
  (let ((next (pop *place-queue*)))
    (if (null next)
        (setf *place-busy* nil)
        (destructuring-bind (key latitude longitude &rest callbacks) next
          (setf *place-busy* t)
          (unless *geocoder*
            (setf *geocoder* (objc:alloc-init-object "CLGeocoder")))
          (let ((location (objc:invoke (objc:invoke "CLLocation" "alloc")
                                       "initWithLatitude:longitude:"
                                       (float latitude 1d0) (float longitude 1d0)))
                (done nil))
            (flet ((answer (name)
                     (unless done
                       (setf done t)
                       ;; Callbacks waiting on the same position meanwhile.
                       (let ((more (assoc key *place-queue* :test #'equal)))
                         (when more
                           (setf *place-queue* (remove more *place-queue*))
                           (setf callbacks (append callbacks (cdddr more)))))
                       (finish-place key name callbacks)
                       (after-delay *place-interval* #'next-place))))
              (objc:with-objc-block (block '(:void (objc:objc-object-pointer objc:objc-object-pointer))
                                      (lambda (placemarks error)
                                        (handler-case
                                            (answer (and (null-object-p error)
                                                         (not (null-object-p placemarks))
                                                         (plusp (objc:invoke placemarks "count"))
                                                         (placemark-name
                                                          (objc:invoke placemarks "objectAtIndex:" 0))))
                                          (error (condition)
                                            (note "place: ~a" condition)
                                            (answer nil)))))
                (objc:invoke *geocoder* "reverseGeocodeLocation:completionHandler:" location block))
              (objc:release location)))))))

(defun place-lookups-pending-p ()
  (or *place-busy* *place-queue*))
