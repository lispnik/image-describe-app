;;;; describe.lisp -- Vision's labels and text as a description to review.
;;;;
;;;; Vision names things in a hierarchy, and the names come back together with
;;;; one confidence: animal, bird, raptor, eagle, all 0.99.  The description
;;;; puts the most particular first -- "Eagle, raptor, bird, animal" -- since
;;;; that is what a person would say, and keeps the rest because each is
;;;; something a search might be for.  Text found in the image follows.
;;;;
;;;; The description is only a proposal.  It is what the user accepts or edits,
;;;; and what is written is whatever they leave, so nothing here may assume the
;;;; text it is given back has the shape it was made in.

(in-package #:image-describe)

(defvar *default-minimum-confidence* 0.3
  "The least confidence a label needs to be in the proposed description.")

(defparameter *most-labels* 10
  "No proposal names more things than this.")

(defparameter *minimum-text-confidence* 0.5
  "Recognised lines less confident than this are left out.")

(defparameter *most-text* 200
  "Characters of recognised text, at most, in a proposal.")

(defun humanize-label (identifier)
  "Vision's identifier as words: \"blue_sky\" is \"blue sky\"."
  (substitute #\Space #\_ identifier))

(defparameter *broad-labels*
  '("raptor" "songbird" "waterfowl" "bird"
    "canine" "feline" "ungulates" "rodent" "primate" "mammal"
    "reptile" "amphibian" "fish" "arthropods" "insect" "invertebrate" "animal"
    "flower" "tree" "plant" "foliage"
    "dessert" "fruit" "vegetable" "meal" "food" "drink"
    "building" "structure" "vehicle" "machine" "tool" "clothing" "furniture"
    "blue_sky" "sky" "liquid" "water" "land" "outdoor" "indoor"
    "adult" "child" "people" "material" "document")
  "Labels Vision gives with a narrower one, broadest last.  Within a family,
these follow the others, in this order.")

(defun broadness (label)
  (or (position (car label) *broad-labels* :test #'string-equal) -1))

(defun label-runs (labels &key (tie 0.005))
  "LABELS, most confident first, in runs whose confidences are within TIE of
the first in the run: a family, as Vision answers one."
  (let ((runs '()) (run '()))
    (dolist (label labels)
      (if (and run (> (abs (- (cdr (first run)) (cdr label))) tie))
          (progn (push (nreverse run) runs) (setf run (list label)))
          (push label run)))
    (when run (push (nreverse run) runs))
    (nreverse runs)))

(defun particular-first (labels)
  "LABELS, (identifier . confidence) most confident first as Vision answers
them, with the broad names in each family after the narrow ones: Vision
answers a family together, with one confidence, and alphabetically, which is
\"animal, bird, eagle, raptor\" and not what a person would say."
  (loop for run in (label-runs labels)
        append (stable-sort (copy-list run) #'< :key #'broadness)))

(defun chosen-labels (labels minimum-confidence)
  "The labels for a description: those at least MINIMUM-CONFIDENCE, or if
there are none, the most confident family if it is a third of that."
  (let ((kept (remove-if (lambda (label) (< (cdr label) minimum-confidence)) labels)))
    (when (and (null kept) labels
               (>= (cdr (first labels)) (/ minimum-confidence 3)))
      (setf kept (first (label-runs labels))))
    (let ((ordered (particular-first kept)))
      (subseq ordered 0 (min *most-labels* (length ordered))))))

(defun shorten (text limit)
  "TEXT no longer than LIMIT, cut at a space where there is one, with an
ellipsis if cut."
  (if (<= (length text) limit)
      text
      (let ((space (position #\Space text :end limit :from-end t)))
        (concatenate 'string (string-right-trim " " (subseq text 0 (or space limit))) "…"))))

(defun collapse-spaces (text)
  "TEXT with each run of white space one space, and none at the ends."
  (with-output-to-string (out)
    (let ((space nil) (started nil))
      (loop for char across text
            do (if (member char '(#\Space #\Tab #\Newline #\Return))
                   (setf space t)
                   (progn (when (and space started) (write-char #\Space out))
                          (write-char char out)
                          (setf space nil started t)))))))

(defun found-text (analysis)
  (let ((lines (loop for (line . confidence) in (analysis-text analysis)
                     when (and (>= confidence *minimum-text-confidence*)
                               (>= (length (string-trim " " line)) 2))
                       collect (collapse-spaces line))))
    (and lines (shorten (format nil "~{~a~^ ~}" lines) *most-text*))))

(defun capitalize-first (text)
  (if (plusp (length text))
      (concatenate 'string (string (char-upcase (char text 0))) (subseq text 1))
      text))

(defun compose-description (analysis &key (minimum-confidence *default-minimum-confidence*)
                                          (include-text t))
  "The description ANALYSIS proposes: what is in the image, most particular
first, then any text read from it.  An empty string if nothing was found."
  (let ((labels (mapcar (lambda (label) (humanize-label (car label)))
                        (chosen-labels (analysis-labels analysis) minimum-confidence)))
        (text (and include-text (found-text analysis))))
    (capitalize-first
     (format nil "~{~a~^, ~}~:[~;. ~]~@[Text: “~a”~]"
             labels (and labels text) text))))

(defun text-start (description)
  "Where \"Text:\" begins in DESCRIPTION: at its start or after a sentence."
  (let ((at (search "text:" description :test #'char-equal)))
    (loop while at
          do (when (or (zerop at)
                       (let ((before (string-right-trim " " (subseq description 0 at))))
                         (or (zerop (length before))
                             (member (char before (1- (length before))) '(#\. #\; #\,)))))
               (return at))
             (setf at (search "text:" description :test #'char-equal :start2 (1+ at))))))

(defun word-count (text)
  (1+ (count #\Space (collapse-spaces text))))

(defun description-keywords (description)
  "The keywords written with DESCRIPTION: its comma- or semicolon-separated
pieces, before any \"Text:\", that are a few words long -- the names of
things, not sentences.  Each once, whatever its case."
  (let* ((end (or (text-start description) (length description)))
         (pieces '())
         (start 0))
    (loop for i from 0 to end
          do (when (or (= i end) (member (char description i) '(#\, #\;)))
               (push (subseq description start i) pieces)
               (setf start (1+ i))))
    (let ((keywords '()))
      (dolist (piece (nreverse pieces))
        (let ((keyword (string-trim '(#\Space #\Tab #\Newline #\. #\“ #\” #\")
                                    (collapse-spaces piece))))
          (when (and (plusp (length keyword))
                     (<= (word-count keyword) 4)
                     (<= (length keyword) 64)
                     (not (member keyword keywords :test #'string-equal)))
            (push keyword keywords))))
      (nreverse keywords))))
