;;;; xattr-tests.lisp -- the description on the file, as Spotlight reads it.

(in-package #:image-describe/tests)

(def-suite xattr :in all-tests :description "Extended attributes.")
(in-suite xattr)

(test a-description-is-written-as-comment-and-keywords
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "Ünïcode [1].png") "")))
      (is (null (id:read-description file)))
      (id:write-description file "Eagle, raptor, bird, animal. Text: “Hi”")
      (is (string= "Eagle, raptor, bird, animal. Text: “Hi”" (id:read-description file)))
      ;; What the xattr command and plutil, not this program, find there.
      (is (equal "Eagle, raptor, bird, animal. Text: “Hi”"
                 (xattr-value file id:+comment-attribute+)))
      (is (equal '("Eagle" "raptor" "bird" "animal")
                 (xattr-value file id:+keywords-attribute+))))))

(test writing-again-replaces-and-blank-removes
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "a.jpg") "")))
      (id:write-description file "Dog, beach")
      (id:write-description file "  A sentence about a dog running along the beach  ")
      (is (string= "A sentence about a dog running along the beach" (id:read-description file)))
      ;; No keywords in a sentence, so none are left from before.
      (is (null (xattr-value file id:+keywords-attribute+)))
      (id:write-description file "   ")
      (is (null (id:read-description file)))
      (is (null (xattr-value file id:+comment-attribute+)))
      ;; Removing what is not there is not an error.
      (finishes (id:remove-description file)))))

(test a-comment-finder-wrote-is-read
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "a.png") "")))
      ;; Finder writes a binary plist; plutil makes one the same way.
      (let ((octets (plutil-binary "\"Typed into Get Info\"")))
        (id::set-attribute file id:+comment-attribute+ octets))
      (is (string= "Typed into Get Info" (id:read-description file))))))

(test something-else-in-the-comment-is-no-description
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "a.png") "")))
      (id::set-attribute file id:+comment-attribute+ (map 'vector #'char-code "raw bytes"))
      (is (null (id:read-description file)))
      (id::set-attribute file id:+comment-attribute+ (plutil-binary "{\"a\":1}"))
      (is (null (id:read-description file))))))

(test a-file-that-cannot-be-written-says-why
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "locked.png") "")))
      (sb-posix:chmod file #o444)
      (let ((condition (handler-case (progn (id:write-description file "Dog") nil)
                         (error (condition) condition))))
        (is (not (null condition)))
        (is (search "Permission denied" (princ-to-string condition)))))
    (signals error (id:write-description (path dir "missing.png") "Dog"))
    (is (null (id:read-description (path dir "missing.png"))))))
