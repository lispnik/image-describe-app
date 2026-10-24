;;;; describe-tests.lisp -- Vision's findings as a description to review.

(in-package #:image-describe/tests)

(def-suite describe :in all-tests :description "Proposing descriptions and keywords.")
(in-suite describe)

(defun analysis (labels &optional text)
  (id:make-analysis :labels labels :text text))

(defparameter *eagle*
  ;; As Vision answers: a family together, alphabetically, one confidence.
  '(("animal" . 0.99d0) ("bird" . 0.99d0) ("eagle" . 0.99d0) ("raptor" . 0.99d0)
    ("outdoor" . 0.41d0) ("sky" . 0.41d0) ("blue_sky" . 0.408d0)
    ("plant" . 0.12d0)))

(test the-particular-comes-before-the-general
  (is (string= "Eagle, raptor, bird, animal, blue sky, sky, outdoor"
               (id:compose-description (analysis *eagle*))))
  (is (string= "Zebra, ungulates, mammal, animal"
               (id:compose-description
                (analysis '(("animal" . 0.98d0) ("mammal" . 0.98d0)
                            ("ungulates" . 0.98d0) ("zebra" . 0.98d0)))))))

(test the-minimum-confidence-leaves-the-unsure-out
  (is (string= "Eagle, raptor, bird, animal"
               (id:compose-description (analysis *eagle*) :minimum-confidence 0.5)))
  (is (search "plant" (id:compose-description (analysis *eagle*) :minimum-confidence 0.1))))

(test with-nothing-sure-the-best-family-is-kept
  (is (string= "Parrot, bird, animal"
               (id:compose-description
                (analysis '(("animal" . 0.23d0) ("bird" . 0.23d0) ("parrot" . 0.229d0)
                            ("plant" . 0.06d0))))))
  ;; But not when it is a guess.
  (is (string= "" (id:compose-description (analysis '(("material" . 0.06d0))))))
  (is (string= "" (id:compose-description (analysis '())))))

(test no-more-than-ten-things
  (let ((labels (loop for i below 30 collect (cons (format nil "thing~d" i) (- 0.9d0 (* i 0.02d0))))))
    (is (= 10 (length (id:description-keywords (id:compose-description (analysis labels))))))))

(test underscores-are-spaces
  (is (string= "blue sky" (id:humanize-label "blue_sky")))
  (is (string= "Straw hay" (id:compose-description (analysis '(("straw_hay" . 0.8d0)))))))

(test text-found-follows-the-labels
  (let ((found (analysis '(("sign" . 0.9d0) ("outdoor" . 0.5d0))
                         '(("STOP" . 1.0d0) ("Main  Street" . 0.9d0) ("smudge" . 0.2d0) ("x" . 1.0d0)))))
    (is (string= "Sign, outdoor. Text: “STOP Main Street”" (id:compose-description found)))
    (is (string= "Sign, outdoor" (id:compose-description found :include-text nil))))
  (is (string= "Text: “Hello”" (id:compose-description (analysis '() '(("Hello" . 1.0d0)))))))

(test long-text-is-cut-at-a-word
  (let* ((line (format nil "~{~a~^ ~}" (loop repeat 100 collect "word")))
         (description (id:compose-description (analysis '() (list (cons line 1.0d0))))))
    (is (< (length description) 220))
    (is (search "word…”" description))))

(test keywords-are-the-names-of-things
  (is (equal '("Eagle" "raptor" "bird" "animal")
             (id:description-keywords "Eagle, raptor, bird, animal")))
  ;; Not the text read from the picture, which is in the comment.
  (is (equal '("Sign" "outdoor")
             (id:description-keywords "Sign, outdoor. Text: “STOP, Main Street”")))
  ;; Typed by someone: semicolons, spaces, a repeat, a sentence.
  (is (equal '("Rex" "beach" "dog")
             (id:description-keywords
              " Rex;  beach , dog, Dog,  our dog running along the beach at sunset ")))
  (is (equal '("Grandma's garden") (id:description-keywords "Grandma's garden.")))
  (is (null (id:description-keywords "")))
  (is (null (id:description-keywords "Text: “Only text”")))
  ;; "Text:" only counts where a sentence begins.
  (is (equal '("context: maps") (id:description-keywords "context: maps"))))
