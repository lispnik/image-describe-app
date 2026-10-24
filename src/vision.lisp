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

(defun ensure-vision ()
  "Bring up the Objective-C runtime with Vision and CoreGraphics in it."
  (objc:ensure-objc-initialized
   :modules (list +foundation-framework+ +core-graphics-framework+ +vision-framework+)))

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
  `(cffi:foreign-funcall-pointer (foreign-function ,name) () ,@arguments-and-types))

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

;;; Asking Vision --------------------------------------------------------------------

(defstruct analysis
  (labels '())         ; ((identifier . confidence) ...), most confident first
  (text '())           ; ((line . confidence) ...), top to bottom
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

(defun image-io-readable-p (path)
  "Whether ImageIO finds an image in the file at PATH."
  (let* ((url (objc:invoke "NSURL" "fileURLWithPath:" path))
         (source (ccall "CGImageSourceCreateWithURL" :pointer url :pointer (cffi:null-pointer)
                        :pointer)))
    (unless (cffi:null-pointer-p source)
      (unwind-protect (plusp (ccall "CGImageSourceGetCount" :pointer source :size))
        (cf-release source)))))

(defun make-handler (path)
  "A VNImageRequestHandler for the image at PATH, and which reader it used.
IM first; if it cannot read the file, ImageIO by way of the URL."
  (multiple-value-bind (image orientation)
      (handler-case (load-for-vision path)
        (error (condition)
          (unless (image-io-readable-p path)
            (error "Not an image that can be read: ~a."
                   (without-path (condition-text condition) path)))
          (values nil nil)))
    (if image
        (let ((cg (unwind-protect (cg-image-from-rgb image)
                    (im:destroy image))))
          (unwind-protect
               (values (objc:invoke (objc:invoke "VNImageRequestHandler" "alloc")
                                    "initWithCGImage:orientation:options:"
                                    cg orientation (empty-dictionary))
                       :im)
            ;; The handler holds its own reference.
            (cf-release cg)))
        (values (objc:invoke (objc:invoke "VNImageRequestHandler" "alloc")
                             "initWithURL:options:"
                             (objc:invoke "NSURL" "fileURLWithPath:" path)
                             (empty-dictionary))
                :image-io))))

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

(defun analyze-image (path &key (read-text t))
  "What Vision finds in the image at native PATH, as an ANALYSIS.  With
READ-TEXT, the text in it too.  Signals if the file cannot be read."
  (unless (probe-file (sb-ext:parse-native-namestring path))
    (error "The file is not there any more."))
  (ensure-vision)
  (objc:with-autorelease-pool ()
    (multiple-value-bind (handler source) (make-handler path)
      (let ((classify (objc:alloc-init-object "VNClassifyImageRequest"))
            (text (and read-text (objc:alloc-init-object "VNRecognizeTextRequest"))))
        (unwind-protect
             (progn
               (when text
                 (objc:invoke text "setRecognitionLevel:" 0) ; accurate
                 (objc:invoke text "setUsesLanguageCorrection:" t))
               (objc:invoke-with-error handler "performRequests:error:"
                                       (if text (vector classify text) (vector classify)))
               (make-analysis
                :labels (stable-sort (results-list classify #'label-of) #'> :key #'cdr)
                :text (and text (results-list text #'line-of))
                :source source))
          (objc:release classify)
          (when text (objc:release text))
          (objc:release handler))))))
