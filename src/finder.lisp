;;;; finder.lisp -- comments Finder's Get Info window shows.
;;;;
;;;; Get Info reads a file's comment from the .DS_Store of its folder, not from
;;;; the kMDItemFinderComment attribute Spotlight reads, and only Finder
;;;; writes .DS_Store files safely: it keeps them in memory and writes them
;;;; back over anything else wrote.  So when chosen, the comment is also given
;;;; to Finder to set, by AppleScript, which writes both.  macOS asks the user
;;;; once whether this application may control Finder.

(in-package #:image-describe)

(defun applescript-string (string)
  "STRING as an AppleScript string literal."
  (with-output-to-string (out)
    (write-char #\" out)
    (loop for char across string
          do (case char
               (#\" (write-string "\\\"" out))
               (#\\ (write-string "\\\\" out))
               (#\Newline (write-string "\\n" out))
               (#\Tab (write-string "\\t" out))
               (t (write-char char out))))
    (write-char #\" out)))

(defun finder-comment-script (paths-and-comments)
  (with-output-to-string (out)
    (format out "tell application \"Finder\"~%")
    (loop for (path . comment) in paths-and-comments
          do (format out "  set comment of (POSIX file ~a as alias) to ~a~%"
                     (applescript-string path) (applescript-string (or comment ""))))
    (format out "end tell~%")))

(defun set-finder-comments (paths-and-comments)
  "Have Finder set the comment of each (PATH . COMMENT), NIL or \"\" for
none.  For the main thread.  Signals with Finder's or macOS's reason if it
could not -- most often that this application may not control Finder."
  (when paths-and-comments
    (objc:with-autorelease-pool ()
      (let ((script (objc:invoke (objc:invoke "NSAppleScript" "alloc") "initWithSource:"
                                 (finder-comment-script paths-and-comments))))
        (unwind-protect
             (cffi:with-foreign-object (info :pointer)
               (setf (cffi:mem-ref info :pointer) (cffi:null-pointer))
               (let ((result (objc:invoke script "executeAndReturnError:" info))
                     (error-info (cffi:mem-ref info :pointer)))
                 (when (and (null-object-p result) (not (null-object-p error-info)))
                   (let ((number (property error-info "NSAppleScriptErrorNumber"))
                         (message (property error-info "NSAppleScriptErrorMessage")))
                     (error "Finder did not set the comments: ~a~@[ (~d)~]"
                            (cond ((eql number -1743d0)
                                   "Image Describer may not control Finder. Allow it in System Settings > Privacy & Security > Automation")
                                  (message (string-right-trim "." message))
                                  (t "no reason given"))
                            (and number (round number)))))
                 t))
          (objc:release script))))))
