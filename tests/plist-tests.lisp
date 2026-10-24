;;;; plist-tests.lisp -- binary property lists, checked against plutil.

(in-package #:image-describe/tests)

(def-suite plist :in all-tests :description "Binary property lists.")
(in-suite plist)

(defun round-trip (object)
  (id:decode-binary-plist (id:encode-binary-plist object)))

(test strings-and-lists-come-back
  (dolist (object (list "" "a" "Eagle, raptor, bird, animal"
                        (make-string 300 :initial-element #\x)
                        "Café “quoted” — dash" "An eagle 🦅 in flight"
                        '() '("one") '("Eagle" "raptor" "bird" "animal")
                        '("ünïcode" "🦅" "plain")))
    (is (equal object (round-trip object)) "~s" object)))

(test enough-objects-for-two-byte-references
  (let ((keywords (loop for i below 300 collect (format nil "keyword ~d" i))))
    (is (equal keywords (round-trip keywords)))))

(test plutil-reads-what-is-written
  (dolist (object (list "Eagle, raptor, bird, animal" '("Eagle" "bird") '() ""
                        ;; Outside ASCII, which bplist00 stores as UTF-16, and past the BMP.
                        "Café 🦅 <&>" '("ünï" "🦅")
                        (make-string 70000 :initial-element #\y)
                        (loop for i below 300 collect (format nil "k~d" i))))
    (is (equal object (plutil-reading (id:encode-binary-plist object)))
        "~s" (if (stringp object) (subseq object 0 (min 40 (length object))) object))))

(test what-plutil-writes-is-read
  (is (string= "A comment typed in Finder" (id:decode-binary-plist
                                            (plutil-binary "\"A comment typed in Finder\""))))
  (is (string= "Grüße 🦅" (id:decode-binary-plist (plutil-binary "\"Grüße 🦅\""))))
  (is (equal '("a" "b" "c") (id:decode-binary-plist (plutil-binary "[\"a\",\"b\",\"c\"]")))))

(test anything-else-reads-as-nil
  (is (null (id:decode-binary-plist (plutil-binary "{\"a\":\"b\"}"))))
  (is (null (id:decode-binary-plist (plutil-binary "[1,2]"))))
  (is (null (id:decode-binary-plist #())))
  (is (null (id:decode-binary-plist (map 'vector #'char-code "not a property list at all, really"))))
  ;; Cut short, and with its trailer pointing nowhere.
  (let ((octets (id:encode-binary-plist "Eagle, raptor, bird, animal")))
    (is (null (id:decode-binary-plist (subseq octets 0 30))))
    (let ((broken (copy-seq octets)))
      (setf (aref broken (- (length broken) 1)) 250)
      (is (null (id:decode-binary-plist broken))))))

(test only-strings-are-encoded
  (signals error (id:encode-binary-plist 42))
  (signals error (id:encode-binary-plist '("a" 2))))
