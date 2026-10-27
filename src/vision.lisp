;;;; vision.lisp -- what is in an image, asked of Apple's Vision framework.
;;;;
;;;; On the Mac, without a network: VNClassifyImageRequest names what it sees
;;;; ("animal", "bird", "eagle", "sky") with a confidence for each, and
;;;; VNRecognizeTextRequest reads any text.  -performRequests:error: is
;;;; synchronous, so the requests hold their results when it returns and no
;;;; block is needed.
;;;;
;;;; IM reads the image, which is what lets this take PCX, SGI, TGA and the
;;;; rest of IM's formats, and makes it small: Vision looks at a few hundred
;;;; pixels across for its labels, and a 6000-pixel photograph would cost a
;;;; hundred megabytes and a second to hand it.  The pixels go to Vision as a
;;;; CGImage made with CoreGraphics' C functions.  What IM cannot read --
;;;; camera RAW, WebP -- Vision is given by its URL, and ImageIO reads it.
;;;;
;;;; This runs on the batch's worker threads (batch.lisp).  Vision, CoreGraphics
;;;; and the Objective-C bridge are all safe there; AppKit is not, and nothing
;;;; here touches it.  Each image is identified inside an autorelease pool of
;;;; its own, so that a long batch does not keep every image's results.

(in-package #:image-describe)

(defparameter +foundation-framework+
  "/System/Library/Frameworks/Foundation.framework/Versions/C/Foundation")
(defparameter +vision-framework+
  "/System/Library/Frameworks/Vision.framework/Versions/A/Vision")
(defparameter +core-graphics-framework+
  "/System/Library/Frameworks/CoreGraphics.framework/Versions/A/CoreGraphics")
(defparameter +core-image-framework+
  "/System/Library/Frameworks/CoreImage.framework/Versions/A/CoreImage")
(defparameter +core-location-framework+
  "/System/Library/Frameworks/CoreLocation.framework/Versions/A/CoreLocation")

(defvar *vision-ready* nil
  "Whether ENSURE-VISION has run in this process.")

(defun forget-vision () (setf *vision-ready* nil))
(pushnew 'forget-vision sb-ext:*save-hooks*)

(defun ensure-vision ()
  "Bring up the Objective-C runtime with Vision, CoreGraphics, CoreImage and
CoreLocation in it.  Once a process: it is called for every image, and the
bridge's own initialisation takes a lock and opens each framework again."
  (unless *vision-ready*
    (objc:ensure-objc-initialized
     :modules (list +foundation-framework+ +core-graphics-framework+ +core-image-framework+
                    +core-location-framework+ +vision-framework+))
    (setf *vision-ready* t)))

;;; CoreGraphics and CoreFoundation, by address -----------------------------------
;;;
;;; Looked up when first called rather than linked when this is compiled: the
;;; frameworks are loaded by ENSURE-VISION, after compiling, and again in the
;;; bundle, at addresses of their own.

(defvar *foreign-functions* (make-hash-table :test #'equal :synchronized t))

(defun forget-foreign-functions ()
  (clrhash *foreign-functions*))

(pushnew 'forget-foreign-functions sb-ext:*save-hooks*)

(defun foreign-function (name)
  (or (gethash name *foreign-functions*)
      (setf (gethash name *foreign-functions*)
            (or (cffi:foreign-symbol-pointer name)
                (error "~a is not loaded." name)))))

(defmacro ccall (name &rest arguments-and-types)
  "Call the C function NAME with floating point traps masked, as the
Objective-C bridge does around every send: SBCL traps overflow, invalid and
divide-by-zero, and CoreGraphics and ImageIO commit all three in the ordinary
course of things -- writing a JPEG with an orientation is an overflow."
  `(sb-int:with-float-traps-masked (:overflow :invalid :divide-by-zero :inexact :underflow)
     (cffi:foreign-funcall-pointer (foreign-function ,name) () ,@arguments-and-types)))

(defvar *log* nil
  "Where NOTE writes: the application's log, once MAIN has said so.")

(defun note (control &rest arguments)
  "Write a line to the log."
  (when *log*
    (ignore-errors
     (multiple-value-bind (second minute hour) (get-decoded-time)
       (format *log* "~&~2,'0d:~2,'0d:~2,'0d ~?~%" hour minute second control arguments))
     (finish-output *log*))))

(defun condition-text (condition)
  "CONDITION's report, or its type if the report itself fails."
  (or (ignore-errors (princ-to-string condition))
      (format nil "~a" (type-of condition))))

(defun null-object-p (object)
  "Whether OBJECT is no Objective-C object at all."
  (or (null object) (cffi:null-pointer-p object)))

(defun cf-release (pointer)
  (unless (cffi:null-pointer-p pointer)
    (ccall "CFRelease" :pointer pointer :void)))

;;; Pixels ------------------------------------------------------------------------

(defparameter *largest-side* 1600
  "The longest side, in pixels, an image is made no larger than for Vision:
enough for labels and for reading ordinary text in a photograph.")

(defun rgb-of (image)
  "IMAGE as 8-bit RGB: IMAGE itself if it is already, else a new image, and
IMAGE destroyed.  Not CONVERT-COLOR-SPACE from RGB to RGB, which copies
nothing and leaves the new image black."
  (cond ((and (eq (im:color-space image) :color-space-rgb)
              (eq (im:data-type image) :data-type-byte))
         image)
        (t
         (let ((rgb (im:create (im:width image) (im:height image)
                               :color-space-rgb :data-type-byte)))
           (handler-bind ((error (lambda (condition)
                                   (declare (ignore condition))
                                   (im:destroy rgb))))
             (if (eq (im:data-type image) :data-type-byte)
                 (im:convert-color-space image rgb)
                 (im:convert-to-bitmap image rgb)))
           (im:destroy image)
           rgb))))

(defun fitted-size (width height largest)
  "WIDTH by HEIGHT made no larger than LARGEST on its longer side."
  (let ((scale (min 1 (/ largest (max width height)))))
    (values (max 1 (round (* width scale))) (max 1 (round (* height scale))))))

(defun shrink (image largest)
  "IMAGE no larger than LARGEST on its longer side: IMAGE, or a smaller copy
and IMAGE destroyed."
  (multiple-value-bind (width height) (fitted-size (im:width image) (im:height image) largest)
    (if (and (= width (im:width image)) (= height (im:height image)))
        image
        (let ((small (im:create width height :color-space-rgb :data-type-byte)))
          (handler-bind ((error (lambda (condition)
                                  (declare (ignore condition))
                                  (im:destroy small))))
            (im:resize image small 1))
          (im:destroy image)
          small))))

(defun exif-orientation (image)
  "The EXIF orientation IM read with IMAGE, 1 to 8, or 1 if none: how the
picture is to be turned to be upright."
  (let ((value (ignore-errors (im:image-attribute image "Orientation"))))
    (when (and (typep value 'sequence) (plusp (length value)))
      (setf value (elt value 0)))
    (if (and (integerp value) (<= 1 value 8)) value 1)))

(defvar *disk-lock* (sb-thread:make-mutex :name "reading ahead")
  "Held while a file is read ahead, so that the workers read one file at a
time from the disk.")

(defparameter *read-ahead-limit* (* 256 1024 1024)
  "Files larger than this are not read ahead: they would not all stay cached.")

(defun read-ahead (path)
  "Read the file at PATH once, from start to end, so that it is in memory when
IM reads it.

IM reads some formats in pieces of a few hundred bytes with a system call
each -- its GIF reader walks every block of every frame to count the frames,
a read and a seek for each 255 bytes -- and eight workers doing that on a
spinning disk keep its head seeking: a 50 MB animated GIF on a USB drive took
minutes, while read straight through it takes a second.  One file at a time,
for the same reason.  On an internal SSD this costs a few milliseconds."
  (ignore-errors
   (with-open-file (in (sb-ext:parse-native-namestring path) :element-type '(unsigned-byte 8))
     (when (<= (file-length in) *read-ahead-limit*)
       (let ((buffer (make-array (* 1024 1024) :element-type '(unsigned-byte 8))))
         (sb-thread:with-mutex (*disk-lock*)
           (loop while (plusp (read-sequence buffer in)))))))))

(defun load-for-vision (path &key (largest *largest-side*))
  "The image at native PATH as 8-bit RGB, no larger than LARGEST, and its
EXIF orientation.  The caller destroys the image."
  (read-ahead path)
  (let ((image (im:load (sb-ext:parse-native-namestring path))))
    (let ((orientation (exif-orientation image)))
      (handler-bind ((error (lambda (condition)
                              (declare (ignore condition))
                              (unless (im:destroyed-p image) (im:destroy image)))))
        ;; Smaller first when it can be, so the colour conversion has less to do.
        (when (and (eq (im:data-type image) :data-type-byte)
                   (eq (im:color-space image) :color-space-rgb))
          (setf image (shrink image largest)))
        (setf image (rgb-of image))
        (setf image (shrink image largest)))
      (values image orientation))))

(defun cg-image-from-rgb (image)
  "A new CGImage of IMAGE, 8-bit RGB, which the caller releases.  IM keeps
each colour in a plane of its own with the bottom row first; CoreGraphics
wants them interleaved and the top row first."
  (let* ((width (im:width image))
         (height (im:height image))
         (row (* width 3))
         (size (* row height)))
    (cffi:with-foreign-object (packed :uint8 size)
      (im.ffi::%im-convert-packing (im:plane-pointer image 0) packed width height 3 3
                                   (cffi:foreign-enum-value 'im.ffi::data-type :data-type-byte)
                                   0)
      (let ((data (ccall "CFDataCreateMutable" :pointer (cffi:null-pointer) :long size :pointer))
            (provider (cffi:null-pointer))
            (space (cffi:null-pointer)))
        (unwind-protect
             (progn
               (loop for y from (1- height) downto 0
                     do (ccall "CFDataAppendBytes" :pointer data
                               :pointer (cffi:inc-pointer packed (* y row)) :long row :void))
               (setf provider (ccall "CGDataProviderCreateWithCFData" :pointer data :pointer)
                     space (ccall "CGColorSpaceCreateDeviceRGB" :pointer))
               (let ((cg (ccall "CGImageCreate"
                                :size width :size height
                                :size 8 :size 24 :size row
                                :pointer space
                                :uint32 0 ; kCGImageAlphaNone
                                :pointer provider
                                :pointer (cffi:null-pointer) ; no decode array
                                :bool nil
                                :int 0 ; kCGRenderingIntentDefault
                                :pointer)))
                 (when (cffi:null-pointer-p cg)
                   (error "CoreGraphics could not make an image ~dx~d." width height))
                 cg))
          (cf-release space)
          (cf-release provider)
          (cf-release data))))))

;;; ImageIO ------------------------------------------------------------------------
;;;
;;; For what IM cannot read, for thumbnails, and for a photograph's own record
;;; of when and where it was taken.  Thread-safe, and C: no AppKit.

(defun ns-string (string)
  (objc:invoke "NSString" "stringWithUTF8String:" string))

(defun ns-number (value)
  (etypecase value
    (integer (objc:invoke "NSNumber" "numberWithLong:" value))
    (real (objc:invoke "NSNumber" "numberWithDouble:" (float value 1d0)))
    (boolean (objc:invoke "NSNumber" "numberWithBool:" value))))

(defun cf-key (name)
  "The NSString a framework exports as the constant NAME
(\"kCGImageSourceThumbnailMaxPixelSize\"), or, for a name that is no such
constant (\"{Exif}\", \"Latitude\"), NAME itself."
  (let ((symbol (and (or (char= #\k (char name 0)) (starts-with-p "NS" name))
                     (cffi:foreign-symbol-pointer name))))
    (if symbol
        (cffi:mem-ref symbol :pointer)
        (ns-string name))))

(defun dictionary (&rest keys-and-values)
  "An NSDictionary of KEYS-AND-VALUES, Lisp strings and numbers, or objects."
  (let ((dictionary (objc:invoke "NSMutableDictionary" "dictionary")))
    (loop for (key value) on keys-and-values by #'cddr
          do (objc:invoke dictionary "setObject:forKey:"
                          (if (or (numberp value) (eq value t)) (ns-number value)
                              (if (stringp value) (ns-string value) value))
                          (cf-key key)))
    dictionary))

(defun image-source (path)
  "A new CGImageSource for the file at PATH, which the caller releases, or NIL."
  (let ((source (ccall "CGImageSourceCreateWithURL"
                       :pointer (objc:invoke "NSURL" "fileURLWithPath:" path)
                       :pointer (cffi:null-pointer) :pointer)))
    (if (or (cffi:null-pointer-p source)
            (zerop (ccall "CGImageSourceGetCount" :pointer source :size)))
        (progn (cf-release source) nil)
        source)))

(defun image-io-readable-p (path)
  "Whether ImageIO finds an image in the file at PATH."
  (let ((source (image-source path)))
    (when source (cf-release source) t)))

(defun image-io-thumbnail (path largest)
  "A new CGImage of the image at PATH, upright and no larger than LARGEST,
from ImageIO, which the caller releases; or NIL.  Made from the thumbnail a
camera stored in the file when it is big enough, which is what makes this
fast for a large photograph."
  (let ((source (image-source path)))
    (when source
      (unwind-protect
           (let ((cg (ccall "CGImageSourceCreateThumbnailAtIndex"
                            :pointer source :size 0
                            :pointer (dictionary "kCGImageSourceCreateThumbnailFromImageIfAbsent" t
                                                 "kCGImageSourceCreateThumbnailWithTransform" t
                                                 "kCGImageSourceThumbnailMaxPixelSize" largest)
                            :pointer)))
             (unless (cffi:null-pointer-p cg) cg))
        (cf-release source)))))

(defun lisp-value (object)
  "An NSString or NSNumber as a Lisp string or number, else NIL."
  (cond ((null-object-p object) nil)
        ((objc:invoke-bool object "isKindOfClass:" (objc:coerce-to-objc-class "NSString"))
         (objc:ns-string-to-string object))
        ((objc:invoke-bool object "isKindOfClass:" (objc:coerce-to-objc-class "NSNumber"))
         (objc:invoke object "doubleValue"))))

(defun property (dictionary &rest keys)
  "The value at KEYS, a path of nested dictionary keys, as a Lisp value."
  (let ((value dictionary))
    (dolist (key keys (lisp-value value))
      (when (null-object-p value) (return nil))
      (setf value (objc:invoke value "objectForKey:" (ns-string key))))))

(defun parse-exif-date (string)
  "EXIF's \"2023:07:04 18:22:01\" as (2023 7 4), or NIL."
  (when (and (stringp string) (>= (length string) 10))
    (let ((year (parse-integer string :start 0 :end 4 :junk-allowed t))
          (month (parse-integer string :start 5 :end 7 :junk-allowed t))
          (day (parse-integer string :start 8 :end 10 :junk-allowed t)))
      (and year month day (<= 1826 year 2999) (<= 1 month 12) (<= 1 day 31)
           (list year month day)))))

(defun photo-metadata (path)
  "When and where the photograph at PATH says it was taken: (values (year
month day) (latitude . longitude)), either NIL if it does not say."
  (let ((source (image-source path)))
    (if (null source)
        (values nil nil)
        (unwind-protect
             (let ((properties (ccall "CGImageSourceCopyPropertiesAtIndex"
                                      :pointer source :size 0 :pointer (cffi:null-pointer)
                                      :pointer)))
               (if (cffi:null-pointer-p properties)
                   (values nil nil)
                   (unwind-protect
                        (let ((date (parse-exif-date
                                     ;; Not {TIFF} DateTime: that is when the file
                                     ;; was last changed, not when it was taken.
                                     (or (property properties "{Exif}" "DateTimeOriginal")
                                         (property properties "{Exif}" "DateTimeDigitized"))))
                              (latitude (property properties "{GPS}" "Latitude"))
                              (longitude (property properties "{GPS}" "Longitude")))
                          (values date
                                  (and (realp latitude) (realp longitude)
                                       (not (and (zerop latitude) (zerop longitude)))
                                       (cons (if (equal (property properties "{GPS}" "LatitudeRef") "S")
                                                 (- latitude) latitude)
                                             (if (equal (property properties "{GPS}" "LongitudeRef") "W")
                                                 (- longitude) longitude)))))
                     (cf-release properties))))
          (cf-release source)))))

;;; Asking Vision --------------------------------------------------------------------

(defstruct analysis
  (labels '())         ; ((identifier . confidence) ...), most confident first
  (text '())           ; ((line . confidence) ...), top to bottom
  (animals '())        ; ((name . confidence) ...): "Cat", "Dog"
  (faces 0)            ; how many faces
  (codes '())          ; barcode and QR code payloads
  (date nil)           ; (year month day) the photograph was taken
  (location nil)       ; (latitude . longitude)
  (place nil)          ; where LOCATION is, in words, once looked up
  (source :im))        ; :IM or :IMAGE-IO, whichever read the pixels

(defparameter *lowest-kept-confidence* 0.05
  "Labels less confident than this are not kept at all.  Which of the rest
make the description is chosen later, so that changing the minimum does not
mean identifying again.")

(defun empty-dictionary ()
  (objc:invoke "NSDictionary" "dictionary"))

(defun without-path (message path)
  "MESSAGE less a trailing \": PATH\" and full stop: IM ends each error with
the file's path, which the window already shows."
  (let* ((message (string-right-trim "." message))
         (suffix (format nil ": ~a" path)))
    (if (and (> (length message) (length suffix))
             (string= suffix message :start2 (- (length message) (length suffix))))
        (subseq message 0 (- (length message) (length suffix)))
        message)))

(defun image-for-vision (path)
  "A new CGImage of the image at PATH, no larger than *LARGEST-SIDE*, which
the caller releases; its EXIF orientation; and which reader made it, :IM or
:IMAGE-IO.  IM first; if it cannot read the file, ImageIO, which turns the
picture upright itself."
  (multiple-value-bind (image orientation)
      (handler-case (load-for-vision path)
        (error (condition)
          (let ((cg (image-io-thumbnail path *largest-side*)))
            (unless cg
              (error "Not an image that can be read: ~a."
                     (without-path (condition-text condition) path)))
            (return-from image-for-vision (values cg 1 :image-io)))))
    (values (unwind-protect (cg-image-from-rgb image) (im:destroy image))
            orientation
            :im)))

;;; Requests in flight, so that Stop can cancel them --------------------------------

(defvar *requests-lock* (sb-thread:make-mutex :name "requests in flight"))
(defvar *requests-in-flight* (make-hash-table :test #'eq)
  "Each worker thread's Vision requests, while it performs them.")

(defun cancel-requests (threads)
  "Cancel the Vision requests THREADS are performing: -performRequests:error:
then returns at once, with an error nobody records.  -[VNRequest cancel] is
meant to be sent from another thread."
  (sb-thread:with-mutex (*requests-lock*)
    (dolist (thread threads)
      (dolist (request (gethash thread *requests-in-flight*))
        (ignore-errors (objc:invoke request "cancel"))))))

(defmacro with-requests-in-flight ((requests) &body body)
  `(progn
     (sb-thread:with-mutex (*requests-lock*)
       (setf (gethash sb-thread:*current-thread* *requests-in-flight*) ,requests))
     (unwind-protect (progn ,@body)
       (sb-thread:with-mutex (*requests-lock*)
         (remhash sb-thread:*current-thread* *requests-in-flight*)))))

(defun results-list (request function)
  (let ((results (objc:invoke request "results")))
    (if (null-object-p results)
        '()
        (loop for i below (objc:invoke results "count")
              for item = (funcall function (objc:invoke results "objectAtIndex:" i))
              when item collect item))))

(defun label-of (observation)
  (let ((confidence (objc:invoke observation "confidence")))
    (when (>= confidence *lowest-kept-confidence*)
      (cons (objc:invoke-into 'string observation "identifier")
            (float confidence 1d0)))))

(defun line-of (observation)
  (let ((candidates (objc:invoke observation "topCandidates:" 1)))
    (when (plusp (objc:invoke candidates "count"))
      (let ((candidate (objc:invoke candidates "objectAtIndex:" 0)))
        (cons (objc:invoke-into 'string candidate "string")
              (float (objc:invoke candidate "confidence") 1d0))))))

(defun animal-of (observation)
  "The best of a recognised animal's labels: (\"Cat\" . 0.93)."
  (let ((labels (objc:invoke observation "labels")))
    (when (plusp (objc:invoke labels "count"))
      (label-of (objc:invoke labels "objectAtIndex:" 0)))))

(defun code-of (observation)
  (let ((payload (objc:invoke observation "payloadStringValue")))
    (unless (null-object-p payload)
      (let ((text (objc:ns-string-to-string payload)))
        (and (plusp (length text)) text)))))

(defun analyze-image (path &key (read-text t))
  "What Vision finds in the image at native PATH, as an ANALYSIS: what is in
it, any animals, faces and codes, and with READ-TEXT the text in it; when and
where the photograph says it was taken.  Signals if the file cannot be read."
  (unless (probe-file (sb-ext:parse-native-namestring path))
    (error "The file is not there any more."))
  (ensure-vision)
  (objc:with-autorelease-pool ()
    (multiple-value-bind (cg orientation source) (image-for-vision path)
      (unwind-protect
           (let ((handler (objc:invoke (objc:invoke "VNImageRequestHandler" "alloc")
                                       "initWithCGImage:orientation:options:"
                                       cg orientation (empty-dictionary)))
                 (classify (objc:alloc-init-object "VNClassifyImageRequest"))
                 (animals (objc:alloc-init-object "VNRecognizeAnimalsRequest"))
                 (faces (objc:alloc-init-object "VNDetectFaceRectanglesRequest"))
                 (codes (objc:alloc-init-object "VNDetectBarcodesRequest"))
                 (text (and read-text (objc:alloc-init-object "VNRecognizeTextRequest"))))
             (unwind-protect
                  (let ((requests (remove nil (list classify animals faces codes text))))
                    (when text
                      (objc:invoke text "setRecognitionLevel:" 0) ; accurate
                      (objc:invoke text "setUsesLanguageCorrection:" t))
                    (with-requests-in-flight (requests)
                      (objc:invoke-with-error handler "performRequests:error:"
                                              (coerce requests 'vector)))
                    (multiple-value-bind (date location) (photo-metadata path)
                      (make-analysis
                       :labels (stable-sort (results-list classify #'label-of) #'> :key #'cdr)
                       :text (and text (results-list text #'line-of))
                       :animals (results-list animals #'animal-of)
                       :faces (length (results-list faces #'identity))
                       :codes (remove-duplicates (results-list codes #'code-of)
                                                 :test #'string= :from-end t)
                       :date date
                       :location location
                       :source source)))
               (dolist (request (list classify animals faces codes text))
                 (when request (objc:release request)))
               (objc:release handler)))
        (cf-release cg)))))
