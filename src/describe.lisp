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

(defparameter +months+
  #("January" "February" "March" "April" "May" "June" "July" "August" "September"
    "October" "November" "December"))

(defun when-and-where (analysis &key (include-place t))
  "\"Taken 4 July 2023 in Portland, OR, United States\", or as much of it as
is known, or NIL."
  (let ((date (analysis-date analysis))
        (place (and include-place (analysis-place analysis))))
    (when (or date place)
      (format nil "Taken~@[ ~a~]~@[ in ~a~]"
              (and date (destructuring-bind (year month day) date
                          (format nil "~d ~a ~d" day (aref +months+ (1- month)) year)))
              place))))

(defun people (faces)
  (and (plusp faces) (format nil "~d ~:[people~;person~]" faces (= faces 1))))

(defun with-animals (analysis)
  "The labels, with any animal Vision recognised that they do not name."
  (let ((labels (analysis-labels analysis)))
    (dolist (animal (analysis-animals analysis))
      (let ((name (string-downcase (car animal))))
        (unless (find name labels :key #'car :test #'string-equal)
          (setf labels (merge 'list (list (cons name (cdr animal))) (copy-list labels)
                              #'> :key #'cdr)))))
    labels))

(defun compose-description (analysis &key (minimum-confidence *default-minimum-confidence*)
                                          (include-text t) (include-details t)
                                          (include-place t))
  "The description ANALYSIS proposes, in short sentences: what is in the image, most particular first; how many
people; with INCLUDE-DETAILS, when and where it was taken and any barcode;
then, with INCLUDE-TEXT, any text read from it.  An empty string if nothing
was found."
  (let* ((labels (mapcar (lambda (label) (humanize-label (car label)))
                         (chosen-labels (with-animals analysis) minimum-confidence)))
         (text (and include-text (found-text analysis)))
         (pieces (remove nil
                         (list (and labels (format nil "~{~a~^, ~}" labels))
                               (people (analysis-faces analysis))
                               (and include-details
                                    (when-and-where analysis :include-place include-place))))))
    (when include-details
      (dolist (code (analysis-codes analysis))
        (setf pieces (append pieces (list (format nil "Code: ~a" (shorten (collapse-spaces code) 100)))))))
    (capitalize-first
     (format nil "~{~a~^. ~}~:[~;. ~]~@[Text: “~a”~]"
             (mapcar #'capitalize-first pieces) (and pieces text) text))))

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

(defun split-on (text predicate)
  "TEXT in the pieces between the positions PREDICATE is true of."
  (let ((pieces '()) (start 0))
    (loop for i from 0 to (length text)
          do (when (or (= i (length text)) (funcall predicate i))
               (push (subseq text start i) pieces)
               (setf start (1+ i))))
    (nreverse pieces)))

(defun sentences (text)
  "TEXT's sentences: split at each full stop followed by a space."
  (split-on text (lambda (i) (and (char= #\. (char text i))
                                  (< (1+ i) (length text))
                                  (char= #\Space (char text (1+ i)))))))

(defun starts-with-p (prefix text)
  (and (>= (length text) (length prefix))
       (string-equal prefix text :end2 (length prefix))))

(defun description-keywords (description)
  "The keywords written with DESCRIPTION: the comma- or semicolon-separated
pieces of its sentences that are a few words long -- the names of things,
not sentences -- and the place in a \"Taken ... in ...\" sentence.  Not the
text read from the image, nor a code.  Each once, whatever its case."
  (let ((body (subseq description 0 (or (text-start description) (length description))))
        (keywords '()))
    (dolist (sentence (sentences body))
      (let ((sentence (string-trim '(#\Space #\Tab #\Newline #\.) sentence)))
        (cond ((starts-with-p "Code:" sentence))
              ((starts-with-p "Taken" sentence)
               (let ((in (search " in " sentence)))
                 (when in
                   (setf sentence (subseq sentence (+ in 4))))))
              (t nil))
        (unless (or (starts-with-p "Code:" sentence) (starts-with-p "Taken" sentence))
          (dolist (piece (split-on sentence (lambda (i) (member (char sentence i) '(#\, #\;)))))
            (let ((keyword (string-trim '(#\Space #\Tab #\Newline #\. #\“ #\” #\")
                                        (collapse-spaces piece))))
              (when (and (plusp (length keyword))
                         (<= (word-count keyword) 4)
                         (<= (length keyword) 64)
                         (not (member keyword keywords :test #'string-equal)))
                (push keyword keywords)))))))
    (nreverse keywords)))
