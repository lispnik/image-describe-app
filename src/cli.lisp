;;;; cli.lisp -- the application from a terminal, for scripts.
;;;;
;;;;   image-describe [options] PATH...
;;;;
;;;; Given arguments, the bundle's executable identifies the images in each
;;;; PATH (a folder, or an image) without a window and prints what it would
;;;; write: one line an image, or a JSON object an image with --json.  Nothing
;;;; is written unless --accept says so.  The review is then whatever you do
;;;; with the output: edit the JSON lines and give them back with --apply.
;;;;
;;;; In the bundle, asdf-macos-app sends *STANDARD-OUTPUT* to the
;;;; application's log, so this writes to file descriptors 1 and 2 itself.

(in-package #:image-describe)

(defparameter +usage+
  "usage: image-describe [options] PATH...
       image-describe --apply FILE

Identify the images in each PATH, a folder or an image, and print a
description for each.  Nothing is written onto the images unless --accept.

  -r, --recursive         look in subfolders too
      --again             identify images that already have a description
      --accept            write the descriptions onto the images
      --json              print a JSON object a line: path, state, description
      --no-text           do not read text in the images
      --places            look up place names for GPS positions (network)
      --claude            ask Claude for a sentence too (sends each image;
                          key from ANTHROPIC_API_KEY or the app's Keychain item)
      --tags              with --accept, tag each image with its first keyword
      --min-confidence N  how sure Vision must be of a label, 5 to 95 (30)
      --workers N         images identified at once
      --apply FILE        write the descriptions in FILE, JSON lines as --json
                          prints them (\"-\" for standard input), and stop
  -h, --help              this
")

(defun fd-stream (fd)
  (sb-sys:make-fd-stream fd :output t :external-format :utf-8 :buffering :line))

(defun parse-cli (arguments)
  "ARGUMENTS as a plist of options and a list of paths, or signal."
  (let ((options (list :minimum-confidence *default-minimum-confidence* :read-text t))
        (paths '()))
    (loop while arguments
          do (let ((argument (pop arguments)))
               (flet ((value ()
                        (or (pop arguments) (error "~a needs a value." argument))))
                 (cond ((member argument '("-h" "--help") :test #'string=)
                        (setf (getf options :help) t))
                       ((member argument '("-r" "--recursive") :test #'string=)
                        (setf (getf options :recursive) t))
                       ((string= argument "--again") (setf (getf options :again) t))
                       ((string= argument "--accept") (setf (getf options :accept) t))
                       ((string= argument "--json") (setf (getf options :json) t))
                       ((string= argument "--no-text") (setf (getf options :read-text) nil))
                       ((string= argument "--places") (setf (getf options :places) t))
                       ((string= argument "--claude") (setf (getf options :claude) t))
                       ((string= argument "--tags") (setf (getf options :tags) t))
                       ((string= argument "--apply") (setf (getf options :apply) (value)))
                       ((string= argument "--min-confidence")
                        (let ((n (parse-integer (value) :junk-allowed t)))
                          (unless (and n (<= 5 n 95))
                            (error "--min-confidence takes a number from 5 to 95."))
                          (setf (getf options :minimum-confidence) (/ n 100.0))))
                       ((string= argument "--workers")
                        (let ((n (parse-integer (value) :junk-allowed t)))
                          (unless (and n (plusp n)) (error "--workers takes a number above 0."))
                          (setf (getf options :workers) n)))
                       ((and (plusp (length argument)) (char= #\- (char argument 0))
                             (string/= argument "-"))
                        (error "Unknown option ~a." argument))
                       (t (push argument paths))))))
    (values options (nreverse paths))))

(defun absolute-path (path)
  (if (and (plusp (length path)) (char= #\/ (char path 0)))
      (string-right-trim "/" path)
      (concatenate 'string (native (uiop:getcwd)) (string-right-trim "/" path))))

(defun cli-entries (paths recursive)
  "An entry for each image in PATHS: the images in each folder, and each
image named."
  (let ((entries '()))
    (dolist (path paths)
      (let ((path (absolute-path path)))
        (cond ((uiop:directory-exists-p (directory-name path))
               (setf entries (append entries (coerce (entries-from-directory path :recursive recursive)
                                                     'list))))
              ((probe-file (sb-ext:parse-native-namestring path))
               (let ((canonical (native (truename (sb-ext:parse-native-namestring path)))))
                 (setf entries (append entries
                                       (list (entry-for canonical
                                                        (subseq canonical 0 (position #\/ canonical
                                                                                      :from-end t))))))))
              (t (error "No such file or folder: ~a" path)))))
    (coerce (remove-duplicates entries :key #'entry-path :test #'string= :from-end t) 'vector)))

(defun json-line (&rest keys-and-values)
  "A line of JSON for KEYS-AND-VALUES, strings or NIL."
  (let ((dictionary (objc:invoke "NSMutableDictionary" "dictionary")))
    (loop for (key value) on keys-and-values by #'cddr
          when value
            do (objc:invoke dictionary "setObject:forKey:" (ns-string value) (ns-string key)))
    (let ((data (objc:invoke-with-error "NSJSONSerialization" "dataWithJSONObject:options:error:"
                                        dictionary 8))) ; sorted keys
      (objc:ns-string-to-string
       (objc:invoke (objc:invoke (objc:invoke "NSString" "alloc") "initWithData:encoding:" data 4)
                    "autorelease")))))

(defun read-json-lines (stream)
  "Each line of STREAM that is a JSON object, as an alist of its string fields."
  (loop for line = (read-line stream nil)
        while line
        for trimmed = (string-trim '(#\Space #\Tab #\Return) line)
        when (plusp (length trimmed))
          collect (objc:with-autorelease-pool ()
                    (let* ((bytes (sb-ext:string-to-octets trimmed :external-format :utf-8))
                           (data (cffi:with-pointer-to-vector-data (pointer bytes)
                                   (objc:invoke "NSData" "dataWithBytes:length:" pointer (length bytes))))
                           (object (objc:invoke-with-error "NSJSONSerialization"
                                                           "JSONObjectWithData:options:error:" data 0)))
                      (list (cons "path" (property object "path"))
                            (cons "description" (property object "description")))))))

(defun first-keyword-tags (description)
  (let ((first (first (description-keywords (or description "")))))
    (and first (list first))))

(defun apply-file (file out err &key tags)
  "Write the descriptions FILE gives.  Answers the exit status."
  (let ((records (if (string= file "-")
                     (read-json-lines *standard-input*)
                     (with-open-file (in (sb-ext:parse-native-namestring (absolute-path file))
                                         :external-format :utf-8)
                       (read-json-lines in))))
        (failed 0))
    (dolist (record records)
      (let ((path (cdr (assoc "path" record :test #'string=)))
            (description (or (cdr (assoc "description" record :test #'string=)) "")))
        (if (null path)
            (progn (incf failed) (format err "~&a line without a path~%"))
            (handler-case
                (progn (write-description path description
                                          :tags (if tags (first-keyword-tags description) :keep))
                       (format out "~&~a~:[: removed~;~]~%" path (plusp (length description))))
              (error (condition)
                (incf failed)
                (format err "~&~a: ~a~%" path (condition-text condition)))))))
    (if (zerop failed) 0 1)))

(defun wait-for-places (&key (timeout 120))
  "Run the main run loop until no place lookup is waiting, or TIMEOUT seconds."
  (let ((deadline (+ (get-internal-real-time) (* timeout internal-time-units-per-second))))
    (loop while (and (place-lookups-pending-p) (< (get-internal-real-time) deadline))
          do (objc:with-autorelease-pool ()
               (objc:invoke (objc:invoke "NSRunLoop" "currentRunLoop") "runUntilDate:"
                            (objc:invoke "NSDate" "dateWithTimeIntervalSinceNow:" 0.1d0))))))

(defun run-cli (arguments &key (out (fd-stream 1)) (err (fd-stream 2)))
  "Do what ARGUMENTS ask.  Answers the exit status: 0, 1 if any image could
not be identified or written, 2 if the arguments were wrong."
  (multiple-value-bind (options paths)
      (handler-case (parse-cli arguments)
        (error (condition)
          (format err "~&image-describe: ~a~%~%~a" (condition-text condition) +usage+)
          (return-from run-cli 2)))
    (when (getf options :help)
      (write-string +usage+ out)
      (return-from run-cli 0))
    (ensure-vision)
    (when (getf options :apply)
      (return-from run-cli (apply-file (getf options :apply) out err :tags (getf options :tags))))
    (when (null paths)
      (format err "~&image-describe: no folder or image given~%~%~a" +usage+)
      (return-from run-cli 2))
    (let* ((entries (handler-case (cli-entries paths (getf options :recursive))
                      (error (condition)
                        (format err "~&image-describe: ~a~%" (condition-text condition))
                        (return-from run-cli 2))))
           (rows (loop for entry across entries
                       for row from 0
                       when (or (getf options :again) (null (entry-saved entry)))
                         collect row))
           (caption (and (getf options :claude) #'claude-caption))
           (read-text (getf options :read-text))
           (failed 0))
      (when (and caption (null (api-key)))
        (format err "~&image-describe: --claude needs a key: set ANTHROPIC_API_KEY, or set one in the app~%")
        (return-from run-cli 2))
      (let ((run (start-batch entries
                              (lambda (entry) (identification entry :read-text read-text :caption caption))
                              :begin #'begin-identifying :indices rows
                              :workers (or (getf options :workers) *worker-count* (default-worker-count)))))
        (wait-for-batch run))
      (dolist (row rows)
        (let ((entry (aref entries row)))
          (propose-description entry :minimum-confidence (getf options :minimum-confidence)
                                     :include-text read-text)))
      (when (getf options :places)
        (dolist (row rows)
          (let* ((entry (aref entries row))
                 (analysis (entry-analysis entry))
                 (location (and analysis (analysis-location analysis))))
            (when location
              (lookup-place (car location) (cdr location)
                            (lambda (name)
                              (when name
                                (setf (analysis-place analysis) name)
                                (propose-description entry
                                                     :minimum-confidence (getf options :minimum-confidence)
                                                     :include-text read-text)))))))
        (wait-for-places))
      (loop for entry across entries
            for row from 0
            for asked = (member row rows)
            do (when (and asked (getf options :accept)
                          (member (entry-state entry) '(:proposed :edited)))
                 (unless (accept-entry entry :tags (if (getf options :tags)
                                                       (first-keyword-tags (entry-description entry))
                                                       :keep))
                   (incf failed)))
               (when (eq (entry-state entry) :error) (incf failed))
               (let ((state (state-text (or (entry-state entry) :nothing))))
                 (if (getf options :json)
                     (format out "~a~%" (json-line "path" (entry-path entry) "state" state
                                                   "description" (or (entry-description entry) "")
                                                   "error" (entry-message entry)
                                                   "caption_error" (and (entry-analysis entry)
                                                                        (analysis-caption-error
                                                                         (entry-analysis entry)))))
                     (format out "~a~c~a~c~a~%" (entry-path entry) #\Tab state #\Tab
                             (or (entry-message entry)
                                 (substitute #\Space #\Newline (or (entry-description entry) "")))))))
      (finish-output out)
      (if (zerop failed) 0 1))))

(defun cli-arguments-p (arguments)
  "Whether the program was given arguments to act on, rather than launched
as an application, which Finder and the Dock do with none (or, long ago,
with -psn_...)."
  (some (lambda (argument) (not (uiop:string-prefix-p "-psn_" argument))) arguments))
