;;;; plist.lisp -- binary property lists, the shape Spotlight wants an
;;;; extended attribute's value in.
;;;;
;;;; Spotlight imports a com.apple.metadata:<key> attribute only when its value
;;;; is a bplist00 property list: a string for kMDItemFinderComment, an array
;;;; of strings for kMDItemKeywords.  That is all this writes, and all it needs
;;;; to read back -- the comment Finder or another program left on a file --
;;;; so it handles strings, arrays of them, and enough of the rest to skip it.
;;;; Anything malformed reads as NIL rather than as an error: a file's
;;;; attributes are someone else's data.

(in-package #:image-describe)

;;; Writing ---------------------------------------------------------------------

(defun ascii-string-p (string)
  (every (lambda (char) (< (char-code char) 128)) string))

(defun utf-16-units (string)
  "STRING as UTF-16 code units, with a surrogate pair for each character
beyond the Basic Multilingual Plane."
  (let ((units '()))
    (loop for char across string
          for code = (char-code char)
          do (if (< code #x10000)
                 (push code units)
                 (let ((v (- code #x10000)))
                   (push (+ #xD800 (ash v -10)) units)
                   (push (+ #xDC00 (logand v #x3FF)) units))))
    (nreverse units)))

(defun octets-needed (n)
  "The bytes, 1, 2, 4 or 8, a big-endian unsigned N needs."
  (cond ((< n #x100) 1) ((< n #x10000) 2) ((< n #x100000000) 4) (t 8)))

(defun put-uint (out n size)
  (loop for shift from (* 8 (1- size)) downto 0 by 8
        do (vector-push-extend (ldb (byte 8 shift) n) out)))

(defun put-marker (out kind count)
  "An object's marker byte, KIND in the high nibble, and its COUNT: in the low
nibble below 15, else as an integer object after a nibble of 15."
  (if (< count 15)
      (vector-push-extend (logior (ash kind 4) count) out)
      (let ((size (octets-needed count)))
        (vector-push-extend (logior (ash kind 4) 15) out)
        (vector-push-extend (logior #x10 (ecase size (1 0) (2 1) (4 2) (8 3))) out)
        (put-uint out count size))))

(defun flatten-objects (object)
  "OBJECT and everything in it, in the order they are numbered: OBJECT is 0."
  (let ((objects '()))
    (labels ((walk (object)
               (push object objects)
               (when (listp object)
                 (mapc #'walk object))))
      (walk object))
    (coerce (nreverse objects) 'vector)))

(defun encode-binary-plist (object)
  "OBJECT, a string or a list of strings, as the octets of a bplist00
property list."
  (unless (or (stringp object)
              (and (listp object) (every #'stringp object)))
    (error "Only a string or a list of strings can be encoded, not ~s." object))
  (let* ((objects (flatten-objects object))
         (ref-size (octets-needed (length objects)))
         (out (make-array 64 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
         (offsets (make-array (length objects))))
    (loop for char across "bplist00" do (vector-push-extend (char-code char) out))
    ;; A list's children are numbered straight after it, which is the order
    ;; FLATTEN-OBJECTS lists them in, so its references are 1..n.
    (loop for object across objects
          for index from 0
          do (setf (aref offsets index) (fill-pointer out))
             (etypecase object
               (string
                (if (ascii-string-p object)
                    (progn (put-marker out 5 (length object))
                           (loop for char across object
                                 do (vector-push-extend (char-code char) out)))
                    (let ((units (utf-16-units object)))
                      (put-marker out 6 (length units))
                      (dolist (unit units) (put-uint out unit 2)))))
               (list
                (put-marker out #xA (length object))
                (loop for i from 1 to (length object)
                      do (put-uint out (+ index i) ref-size)))))
    (let* ((table-offset (fill-pointer out))
           (offset-size (octets-needed table-offset)))
      (loop for offset across offsets do (put-uint out offset offset-size))
      ;; The trailer: six unused bytes, the two sizes, the object count, the
      ;; top object and where the offset table starts.
      (dotimes (i 6) (vector-push-extend 0 out))
      (vector-push-extend offset-size out)
      (vector-push-extend ref-size out)
      (put-uint out (length objects) 8)
      (put-uint out 0 8)
      (put-uint out table-offset 8))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

;;; Reading ---------------------------------------------------------------------

(defun get-uint (octets start size)
  (when (> (+ start size) (length octets))
    (error "Truncated property list."))
  (let ((n 0))
    (dotimes (i size n)
      (setf n (logior (ash n 8) (aref octets (+ start i)))))))

(defun units-to-string (octets start count)
  "COUNT big-endian UTF-16 code units from START, as a string."
  (with-output-to-string (out)
    (let ((i 0))
      (loop while (< i count)
            do (let ((unit (get-uint octets (+ start (* 2 i)) 2)))
                 (incf i)
                 (if (and (<= #xD800 unit #xDBFF) (< i count))
                     (let ((low (get-uint octets (+ start (* 2 i)) 2)))
                       (incf i)
                       (write-char (code-char (+ #x10000 (ash (- unit #xD800) 10)
                                                 (- low #xDC00)))
                                   out))
                     (write-char (code-char unit) out)))))))

(defun decode-binary-plist (octets)
  "The string or list of strings that OCTETS, a bplist00 property list, holds,
or NIL if it holds something else or is not one.  Never signals."
  (ignore-errors
   (let ((octets (coerce octets '(simple-array (unsigned-byte 8) (*)))))
     (when (and (> (length octets) 40)
                (every (lambda (char index) (= (char-code char) (aref octets index)))
                       "bplist00" '(0 1 2 3 4 5 6 7)))
       (let* ((trailer (- (length octets) 32))
              (offset-size (aref octets (+ trailer 6)))
              (ref-size (aref octets (+ trailer 7)))
              (count (get-uint octets (+ trailer 8) 8))
              (top (get-uint octets (+ trailer 16) 8))
              (table (get-uint octets (+ trailer 24) 8))
              (depth 0))
         (labels ((offset (ref)
                    (unless (< ref count) (error "Bad reference."))
                    (get-uint octets (+ table (* ref offset-size)) offset-size))
                  (count-at (start low)
                    ;; A count in the low nibble, or an integer object after it.
                    (if (< low 15)
                        (values low (1+ start))
                        (let* ((marker (aref octets (1+ start)))
                               (size (ash 1 (logand marker 15))))
                          (unless (= #x10 (logand marker #xF0)) (error "Bad count."))
                          (values (get-uint octets (+ start 2) size) (+ start 2 size)))))
                  (object (ref)
                    (when (> (incf depth) 32) (error "Too deep."))
                    (prog1
                        (let* ((start (offset ref))
                               (marker (aref octets start))
                               (kind (ash marker -4)))
                          (multiple-value-bind (n data) (count-at start (logand marker 15))
                            (case kind
                              (5 (when (> (+ data n) (length octets)) (error "Truncated."))
                               (map 'string #'code-char (subseq octets data (+ data n))))
                              (6 (units-to-string octets data n))
                              (#xA (loop for i below n
                                         collect (object (get-uint octets (+ data (* i ref-size))
                                                                   ref-size))))
                              (t (error "Not a string or an array.")))))
                      (decf depth))))
           (let ((value (object top)))
             (and (or (stringp value) (every #'stringp value))
                  value))))))))
