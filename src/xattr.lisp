;;;; xattr.lisp -- the description, kept on the file in extended attributes.
;;;;
;;;; Two of them, both of which Spotlight imports and so Finder's search finds:
;;;;
;;;;   com.apple.metadata:kMDItemFinderComment  the description, as written
;;;;   com.apple.metadata:kMDItemKeywords       its comma-separated pieces
;;;;
;;;; Each value is a binary property list (plist.lisp).  The comment is the one
;;;; Finder writes when a comment is typed into Get Info, so a comment someone
;;;; wrote there shows here as the file's description, and replacing it is
;;;; replacing theirs -- which is why a described file is not identified again
;;;; unless asked.  Get Info itself reads its comments from the folder's
;;;; .DS_Store, not from here, so it does not show what this writes; Spotlight
;;;; and the search field do.

(in-package #:image-describe)

(defparameter +comment-attribute+ "com.apple.metadata:kMDItemFinderComment")
(defparameter +keywords-attribute+ "com.apple.metadata:kMDItemKeywords")

(cffi:defcfun ("getxattr" %getxattr) :long
  (path :string) (name :string) (value :pointer) (size :unsigned-long)
  (position :uint32) (options :int))

(cffi:defcfun ("setxattr" %setxattr) :int
  (path :string) (name :string) (value :pointer) (size :unsigned-long)
  (position :uint32) (options :int))

(cffi:defcfun ("removexattr" %removexattr) :int
  (path :string) (name :string) (options :int))

(cffi:defcfun ("strerror" %strerror) :string (errnum :int))

(defun errno ()
  "The errno the last foreign call left, which SBCL saves on return."
  (sb-alien:get-errno))

(defconstant +enoattr+ 93 "No such attribute, on macOS.")

(defun get-attribute (path name)
  "The octets of the attribute NAME of the file at native PATH, or NIL."
  (let ((size (%getxattr path name (cffi:null-pointer) 0 0 0)))
    (when (plusp size)
      (let ((octets (make-array size :element-type '(unsigned-byte 8))))
        (let ((read (cffi:with-pointer-to-vector-data (value octets)
                      (%getxattr path name value size 0 0))))
          (and (= read size) octets))))))

(defun set-attribute (path name octets)
  "Set the attribute NAME of the file at PATH to OCTETS, or signal why not."
  (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
    (unless (zerop (cffi:with-pointer-to-vector-data (value octets)
                     (%setxattr path name value (length octets) 0 0)))
      (error "Could not write ~a: ~a." (subseq name (1+ (position #\: name)))
             (%strerror (errno))))))

(defun remove-attribute (path name)
  "Remove the attribute NAME from the file at PATH.  One that is not there is
already removed."
  (unless (zerop (%removexattr path name 0))
    (let ((errno (errno)))
      (unless (= errno +enoattr+)
        (error "Could not remove ~a: ~a." (subseq name (1+ (position #\: name)))
               (%strerror errno))))))

(defun read-description (path)
  "The description on the file at native PATH, or NIL if it has none."
  (let ((octets (ignore-errors (get-attribute path +comment-attribute+))))
    (let ((value (and octets (decode-binary-plist octets))))
      (and (stringp value)
           (plusp (length (string-trim " " value)))
           value))))

(defun write-description (path description)
  "Put DESCRIPTION on the file at PATH, as its comment and its keywords.  An
empty description removes them.  Signals if the file cannot take them."
  (let ((description (string-trim '(#\Space #\Tab #\Newline) (or description ""))))
    (if (zerop (length description))
        (remove-description path)
        (let ((keywords (description-keywords description)))
          (set-attribute path +comment-attribute+ (encode-binary-plist description))
          (if keywords
              (set-attribute path +keywords-attribute+ (encode-binary-plist keywords))
              (remove-attribute path +keywords-attribute+))))
    description))

(defun remove-description (path)
  "Take the description off the file at PATH."
  (remove-attribute path +comment-attribute+)
  (remove-attribute path +keywords-attribute+)
  nil)
