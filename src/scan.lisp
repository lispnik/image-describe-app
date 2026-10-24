;;;; scan.lisp -- finding the images in a folder.
;;;;
;;;; Paths are native namestrings throughout, not Lisp pathnames: a file named
;;;; "a*b.png" or "[1].jpg" is ordinary on macOS and a wildcard to PARSE-NAMESTRING.

(in-package #:image-describe)

;;; IM's shared state ----------------------------------------------------------------
;;;
;;; Images are read on several threads at once (batch.lisp).  IM is safe for
;;; that but for its static buffers, of which reading touches one: the format
;;; list, which imFormatList answers in.  It is read once, under this lock, and
;;; kept.  (image-converter-app found the others, all in writing, which this
;;; application never does.)

(defvar *im-lock* (sb-thread:make-mutex :name "IM's static buffers"))

(defvar *image-extensions* nil
  "IMAGE-EXTENSIONS, once asked.")

(defun forget-extensions ()
  "Ask IM again in a saved image: the bundle's IM may know formats the build's
did not."
  (sb-thread:with-recursive-lock (*im-lock*)
    (setf *image-extensions* nil)))

(pushnew 'forget-extensions sb-ext:*save-hooks*)

(defparameter +image-io-extensions+
  '("heic" "heif" "webp" "dng" "cr2" "cr3" "nef" "arw" "raf" "orf" "rw2" "psd")
  "Formats IM may not read and ImageIO does, which identifying falls back to.")

(defun parse-extensions (string)
  "IM's \"*.tif;*.tiff;\" as (\"tif\" \"tiff\"), lower case."
  (loop with start = 0
        for end = (position #\; string :start start)
        for piece = (string-trim " " (subseq string start (or end (length string))))
        for name = (string-left-trim "*." piece)
        when (plusp (length name)) collect (string-downcase name)
        while end
        do (setf start (1+ end))))

(defun image-extensions ()
  "Every extension of every format IM has registered, and ImageIO's others:
what counts as an image file."
  (sb-thread:with-recursive-lock (*im-lock*)
    (or *image-extensions*
        (setf *image-extensions*
              (remove-duplicates
               (append (loop for name in (ignore-errors (im:format-list))
                             for info = (ignore-errors (im:format-info name))
                             when info append (parse-extensions (second info)))
                       +image-io-extensions+)
               :test #'string=)))))

;;; Paths --------------------------------------------------------------------------

(defun native (pathname)
  (sb-ext:native-namestring pathname))

(defun directory-name (path)
  "PATH, a native namestring, as a directory pathname."
  (let ((path (string-right-trim "/" path)))
    (sb-ext:parse-native-namestring (concatenate 'string (if (string= path "") "/" path) "/"))))

(defun canonical-path (path)
  "PATH, a native path to a folder, with any symbolic links in it resolved and
no trailing slash: /tmp/x and /private/tmp/x are one folder."
  (let ((path (string-right-trim "/" path)))
    (cond ((zerop (length path)) "/")
          ((let ((found (ignore-errors (probe-file (directory-name path)))))
             (and found (string-right-trim "/" (native found)))))
          (t path))))

(defun file-extension (path)
  "The text after the last dot of PATH's last component, lower case, or NIL."
  (let* ((name (subseq path (1+ (or (position #\/ path :from-end t) -1))))
         (dot (position #\. name :from-end t)))
    (and dot (plusp dot) (< (1+ dot) (length name))
         (string-downcase (subseq name (1+ dot))))))

(defun image-file-p (path &optional (extensions (image-extensions)))
  "Whether PATH has the extension of an image format."
  (let ((extension (file-extension path)))
    (and extension (member extension extensions :test #'string=) t)))

(defun hidden-name-p (path)
  (let ((name (subseq path (1+ (or (position #\/ (string-right-trim "/" path) :from-end t) -1)))))
    (and (plusp (length name)) (char= #\. (char name 0)))))

(defun symlink-p (path)
  (let ((path (string-right-trim "/" path)))
    (ignore-errors
     (eq :symlink (sb-impl::native-file-kind path)))))

(defun package-name-p (path)
  "Whether PATH, a folder, is one Finder shows as a single file -- a Photos
library, an application -- whose insides are not the user's pictures."
  (let ((extension (file-extension (string-right-trim "/" path))))
    (and extension
         (member extension '("photoslibrary" "photolibrary" "app" "bundle" "aplibrary"
                             "migratedphotolibrary" "framework" "plugin")
                 :test #'string=)
         t)))

(defun scan-directory (directory &key recursive (extensions (image-extensions)))
  "The image files in DIRECTORY, a native path, as sorted native paths, which
begin with DIRECTORY's CANONICAL-PATH.

With RECURSIVE, its subdirectories too.  Hidden files and folders are passed
over, and so are packages such as a Photos library.  Symbolic links to folders
are not followed, which is what keeps a link back to a parent from making the
walk endless."
  (let ((root (directory-name (canonical-path directory)))
        (found '()))
    (labels ((walk (dir)
               (dolist (entry (ignore-errors
                               (directory (merge-pathnames uiop:*wild-file-for-directory* dir)
                                          :resolve-symlinks nil)))
                 (let ((path (native entry)))
                   (cond ((hidden-name-p path))
                         ((uiop:directory-pathname-p entry)
                          (when (and recursive
                                     (not (symlink-p path))
                                     (not (package-name-p path)))
                            (walk entry)))
                         ((image-file-p path extensions)
                          (push path found)))))))
      (when (uiop:directory-exists-p root)
        (walk root)))
    (sort found #'string<)))

(defun relative-name (path directory)
  "PATH, which is inside DIRECTORY, relative to it.  DIRECTORY may be written
as it was typed or as its CANONICAL-PATH, which is how SCAN-DIRECTORY answers."
  (flet ((under (directory)
           (let ((directory (concatenate 'string (string-right-trim "/" directory) "/")))
             (and (> (length path) (length directory))
                  (string= directory path :end2 (length directory))
                  (subseq path (length directory))))))
    (or (under directory)
        (under (canonical-path directory))
        path)))
