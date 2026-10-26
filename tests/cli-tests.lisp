;;;; cli-tests.lisp -- the program run from a terminal.

(in-package #:image-describe/tests)

(def-suite cli :in all-tests :description "The command line.")
(in-suite cli)

(defun cli (&rest arguments)
  "Run the command line with ARGUMENTS: its exit status, output and errors."
  (let* ((out (make-string-output-stream))
         (err (make-string-output-stream))
         (status (id::run-cli arguments :out out :err err)))
    (values status (get-output-stream-string out) (get-output-stream-string err))))

(test usage-and-mistakes
  (multiple-value-bind (status out) (cli "--help")
    (is (= 0 status))
    (is (search "usage: image-describe" out)))
  (multiple-value-bind (status out err) (cli "--frobnicate" "/tmp")
    (declare (ignore out))
    (is (= 2 status))
    (is (search "Unknown option --frobnicate" err)))
  (is (= 2 (cli)))
  (is (= 2 (cli "--min-confidence" "500" "/tmp")))
  (is (= 2 (cli "/no/such/folder"))))

(test it-prints-and-writes-nothing-unless-asked
  (with-temporary-directory (dir)
    (make-sample (path dir "a.png"))
    (write-text (path dir "b.png") "not an image")
    (make-sample (path dir "c.png"))
    (id:write-description (path dir "c.png") "Described before")
    (multiple-value-bind (status out) (cli "--json" dir)
      (is (= 1 status))                 ; b.png failed
      (let ((lines (remove "" (uiop:split-string out :separator '(#\Newline)) :test #'string=)))
        (is (= 3 (length lines)))
        (is (search "\"state\":\"saved\"" (third lines)))
        (is (search "\"description\":\"Described before\"" (third lines)))
        (is (search "\"state\":\"error\"" (second lines))))
      (is (null (id:read-description (path dir "a.png")))))))

(test accept-and-apply
  (let ((zebra (account-picture "Zebra")))
    (if (not zebra)
        (skip "No account pictures")
        (with-temporary-directory (dir)
          (copy-file zebra (path dir "zebra.heic"))
          (multiple-value-bind (status out) (cli "--accept" "--tags" (path dir "zebra.heic"))
            (is (= 0 status))
            (is (search "saved" out))
            (is (search "Zebra" (id:read-description (path dir "zebra.heic"))))
            (is (equal '("Zebra") (id::read-tags (path dir "zebra.heic")))))
          ;; The review: edit the JSON and give it back.
          (let ((file (write-text (path dir "edited.jsonl")
                                  (format nil "{\"path\":\"~a\",\"description\":\"Our zebra, Ziggy\"}~%"
                                          (path (id:canonical-path dir) "zebra.heic")))))
            (is (= 0 (cli "--apply" file)))
            (is (string= "Our zebra, Ziggy" (id:read-description (path dir "zebra.heic")))))
          ;; Already described: left alone without --again.
          (multiple-value-bind (status out) (cli dir)
            (is (= 0 status))
            (is (search "Our zebra, Ziggy" out)))))))

(test arguments-make-it-the-command-line
  (is-false (id::cli-arguments-p '()))
  (is-false (id::cli-arguments-p '("-psn_0_12345")))
  (is-true (id::cli-arguments-p '("--help")))
  (is-true (id::cli-arguments-p '("/some/folder"))))
