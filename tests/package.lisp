;;;; package.lisp -- the test package, the root suite, and what tests share.

(defpackage #:image-describe/tests
  (:use #:common-lisp #:fiveam)
  (:local-nicknames (#:id #:image-describe))
  (:export #:all-tests #:run-tests))

(in-package #:image-describe/tests)

(def-suite all-tests :description "Everything.")

(defun run-tests ()
  "Run every test and answer whether they all passed."
  (let ((results (run 'all-tests)))
    (explain! results)
    (results-status results)))

;;; Directories to test in ----------------------------------------------------------

(defun call-with-temporary-directory (function)
  ;; Under /tmp on purpose: /tmp is a link to /private/tmp, so every test here
  ;; works on a folder reached through a link, as an external disk's often is.
  (let ((directory (sb-posix:mkdtemp "/tmp/image-describe-test-XXXXXX")))
    (unwind-protect (funcall function directory)
      ;; A test may have left a file read-only.
      (uiop:run-program (list "chmod" "-R" "u+w" directory) :ignore-error-status t)
      (uiop:delete-directory-tree
       (sb-ext:parse-native-namestring (concatenate 'string directory "/"))
       :validate (lambda (path)
                   (search "image-describe-test-" (sb-ext:native-namestring path)))
       :if-does-not-exist :ignore))))

(defmacro with-temporary-directory ((directory) &body body)
  "Run BODY with DIRECTORY bound to the native name of a new, empty directory
that is removed afterwards."
  `(call-with-temporary-directory (lambda (,directory) ,@body)))

(defun path (directory &rest names)
  (format nil "~a~{/~a~}" directory names))

(defun native-pathname (path)
  (sb-ext:parse-native-namestring path))

(defun write-text (path contents)
  (ensure-directories-exist (native-pathname path))
  (with-open-file (out (native-pathname path) :direction :output :if-exists :supersede)
    (write-string contents out))
  path)

(defun names (paths directory)
  (mapcar (lambda (path) (id:relative-name path directory)) paths))

;;; Images ----------------------------------------------------------------------------
;;;
;;; Drawn here rather than checked in.  Vision says little about a gradient,
;;; but it reads it; for what it names, the pictures macOS ships for user
;;; accounts, which every Mac has (and the tests that need them skip if not).

(defun fill-gradient (image)
  (let* ((width (im:width image))
         (height (im:height image))
         (planes (im:depth image)))
    (dotimes (plane planes)
      (let ((pointer (im:plane-pointer image plane)))
        (dotimes (y height)
          (dotimes (x width)
            (setf (cffi:mem-aref pointer :uint8 (+ x (* y width)))
                  (mod (+ (* x 7) (* y 3) (* plane 80)) 256))))))
    image))

(defun make-sample (path &key (width 64) (height 48) (kind :rgb) (format nil))
  "Write an image to PATH: KIND is :RGB, :GRAY or :MAP (palette)."
  (ensure-directories-exist (native-pathname path))
  (im:with-image (rgb (fill-gradient (im:create width height
                                                (if (eq kind :gray) :color-space-gray :color-space-rgb)
                                                :data-type-byte)))
    (ecase kind
      ((:rgb :gray)
       (im:save rgb (native-pathname path) :format format))
      (:map
       (im:with-image (map (im:create width height :color-space-map :data-type-byte))
         (im:convert-color-space rgb map)
         (im:save map (native-pathname path) :format format)))))
  path)

(defparameter *account-pictures* "/Library/User Pictures/Animals/")

(defun account-picture (name)
  "The path of macOS's account picture NAME (\"Zebra\"), or NIL."
  (let ((path (format nil "~a~a.heic" *account-pictures* name)))
    (and (probe-file path) path)))

(defun copy-file (from to)
  (ensure-directories-exist (native-pathname to))
  (uiop:copy-file (native-pathname from) (native-pathname to))
  to)

(defun text-image (string path &key (font-size 64))
  "Draw STRING black on white with AppKit, and write it to PATH as a PNG."
  (id::ensure-frameworks)
  (objc:with-autorelease-pool ()
    (let* ((font (objc:invoke "NSFont" "boldSystemFontOfSize:" (coerce font-size 'double-float)))
           (attributes (objc:invoke "NSMutableDictionary" "dictionary"))
           (text (objc:invoke "NSString" "stringWithUTF8String:" string)))
      (objc:invoke attributes "setObject:forKey:" font
                   (cffi:mem-ref (cffi:foreign-symbol-pointer "NSFontAttributeName") :pointer))
      (let* ((measured (objc:invoke text "sizeWithAttributes:" attributes))
             (width (+ 48 (ceiling (aref measured 0))))
             (height (+ 48 (ceiling (aref measured 1))))
             (image (objc:invoke (objc:invoke "NSImage" "alloc") "initWithSize:"
                                 (vector (float width 1d0) (float height 1d0)))))
        (objc:invoke image "lockFocus")
        (objc:invoke (objc:invoke "NSColor" "whiteColor") "set")
        (objc:invoke (objc:invoke "NSBezierPath" "bezierPathWithRect:"
                                  (vector 0d0 0d0 (float width 1d0) (float height 1d0)))
                     "fill")
        (objc:invoke text "drawAtPoint:withAttributes:" #(24d0 24d0) attributes)
        (objc:invoke image "unlockFocus")
        (let* ((rep (objc:invoke "NSBitmapImageRep" "imageRepWithData:"
                                 (objc:invoke image "TIFFRepresentation")))
               (png (objc:invoke rep "representationUsingType:properties:" 4
                                 (objc:invoke "NSDictionary" "dictionary"))))
          (ensure-directories-exist (native-pathname path))
          (objc:invoke png "writeToFile:atomically:" path nil)
          (objc:release image)
          path)))))

;;; Asking macOS -------------------------------------------------------------------

(defun command-output (&rest command)
  (uiop:run-program command :output :string :error-output nil :ignore-error-status t))

(defun unescape-xml (text)
  (let ((text text))
    (loop for (from . to) in '(("&lt;" . "<") ("&gt;" . ">") ("&quot;" . "\"") ("&apos;" . "'")
                               ("&amp;" . "&"))
          do (setf text (let ((out (make-string-output-stream)) (start 0))
                          (loop for at = (search from text :start2 start)
                                while at
                                do (write-string text out :start start :end at)
                                   (write-string to out)
                                   (setf start (+ at (length from))))
                          (write-string text out :start start)
                          (get-output-stream-string out))))
    text))

(defun plutil-reading (octets)
  "What plutil makes of OCTETS, a property list: an oracle that is not this
program.  A string, a list of strings, or :INVALID."
  (uiop:with-temporary-file (:pathname file :type "plist")
    (with-open-file (out file :direction :output :if-exists :supersede
                              :element-type '(unsigned-byte 8))
      (write-sequence octets out))
    (multiple-value-bind (xml error status)
        (uiop:run-program (list "plutil" "-convert" "xml1" "-o" "-" (namestring file))
                          :output :string :error-output :string :ignore-error-status t
                          :external-format :utf-8)
      (declare (ignore error))
      (if (/= 0 status)
          :invalid
          (let ((strings (loop with start = 0
                               for open = (search "<string>" xml :start2 start)
                               for close = (and open (search "</string>" xml :start2 open))
                               while close
                               collect (unescape-xml (subseq xml (+ open 8) close))
                               do (setf start close)))
                (body (subseq xml (or (search "<plist" xml) 0))))
            (if (search "<array" body)
                strings
                (first strings)))))))

(defun plutil-binary (json)
  "The bplist00 octets plutil writes for JSON."
  (uiop:with-temporary-file (:pathname file :type "json")
    (with-open-file (out file :direction :output :if-exists :supersede
                              :external-format :utf-8)
      (write-string json out))
    (uiop:run-program (list "plutil" "-convert" "binary1" (namestring file)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let ((octets (make-array (file-length in) :element-type '(unsigned-byte 8))))
        (read-sequence octets in)
        octets))))

(defun xattr-value (path name)
  "The attribute NAME of PATH as the xattr command finds it, read by plutil, or NIL."
  (let ((hex (string-trim '(#\Newline #\Space)
                          (command-output "xattr" "-px" name path))))
    (when (and (plusp (length hex)) (not (search "No such xattr" hex)))
      (let ((octets (map '(vector (unsigned-byte 8))
                         (lambda (pair) (parse-integer pair :radix 16))
                         (remove "" (uiop:split-string
                                     (substitute #\Space #\Newline hex) :separator " ")
                                 :test #'string=))))
        (plutil-reading octets)))))
