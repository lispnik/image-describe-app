;;;; vision-tests.lisp -- what Vision finds, by way of IM's pixels.

(in-package #:image-describe/tests)

(def-suite vision :in all-tests :description "Identifying with Vision.")
(in-suite vision)

(defun label-names (analysis)
  (mapcar #'car (id:analysis-labels analysis)))

(test a-picture-is-named
  (let ((zebra (account-picture "Zebra")))
    (if (not zebra)
        (skip "No account pictures in ~a" *account-pictures*)
        (let ((analysis (id:analyze-image zebra)))
          (is (eq :im (id:analysis-source analysis)))
          (is (member "zebra" (label-names analysis) :test #'string=))
          (is (string= "Zebra, ungulates, mammal, animal" (id:compose-description analysis)))
          ;; Most confident first.
          (is (equal (label-names analysis)
                     (mapcar #'car (sort (copy-list (id:analysis-labels analysis))
                                         #'> :key #'cdr))))))))

(test text-is-read
  (with-temporary-directory (dir)
    (let ((file (text-image "Lisp Machine" (path dir "sign.png"))))
      (let ((analysis (id:analyze-image file)))
        (is (search "Lisp Machine" (format nil "~{~a~^ ~}" (mapcar #'car (id:analysis-text analysis)))))
        (is (search "Text: “Lisp Machine”" (id:compose-description analysis))))
      (is (null (id:analysis-text (id:analyze-image file :read-text nil)))))))

(test every-kind-of-image-im-reads
  (with-temporary-directory (dir)
    (dolist (sample '(("rgb.png" :rgb nil) ("gray.png" :gray nil) ("map.gif" :map "GIF")
                      ("old.pcx" :rgb "PCX") ("big.tif" :rgb "TIFF")))
      (destructuring-bind (name kind format) sample
        (let ((file (make-sample (path dir name) :kind kind :format format
                                 :width (if (string= name "big.tif") 3000 64)
                                 :height (if (string= name "big.tif") 2000 48))))
          (finishes (id:analyze-image file))
          (is (eq :im (id:analysis-source (id:analyze-image file :read-text nil))) name))))))

(test what-im-cannot-read-imageio-may
  ;; A Photoshop file, made by sips, which IM has no reader for.
  (let ((zebra (account-picture "Zebra")))
    (if (not zebra)
        (skip "No account pictures")
        (with-temporary-directory (dir)
          (let ((psd (path dir "zebra.psd")))
            (command-output "sips" "-s" "format" "psd" zebra "--out" psd)
            (if (not (probe-file psd))
                (skip "sips cannot write PSD here")
                (let ((analysis (id:analyze-image psd)))
                  (is (eq :image-io (id:analysis-source analysis)))
                  (is (member "zebra" (label-names analysis) :test #'string=)))))))))

(test a-file-that-is-no-image-says-so
  (with-temporary-directory (dir)
    (let ((file (write-text (path dir "broken.png") "not an image")))
      (let ((condition (handler-case (progn (id:analyze-image file) nil)
                         (error (condition) condition))))
        (is (search "Not an image that can be read" (princ-to-string condition)))
        ;; Without the path, which the window already shows.
        (is (not (search dir (princ-to-string condition))))))
    (signals error (id:analyze-image (path dir "gone.png")))))

(test pixels-are-made-small-and-upright
  (multiple-value-bind (w h) (id::fitted-size 6000 4000 1600)
    (is (= 1600 w)) (is (= 1067 h)))
  (multiple-value-bind (w h) (id::fitted-size 300 200 1600)
    (is (= 300 w)) (is (= 200 h)))
  (with-temporary-directory (dir)
    ;; Top half white and bottom half black, so that a picture turned over shows.
    (let ((file (path dir "half.png")))
      (im:with-image (image (im:create 40 40 :color-space-gray :data-type-byte))
        (let ((pointer (im:plane-pointer image 0)))
          ;; IM's first row is the bottom one.
          (dotimes (y 40)
            (dotimes (x 40)
              (setf (cffi:mem-aref pointer :uint8 (+ x (* y 40))) (if (>= y 20) 255 0)))))
        (im:save image (native-pathname file) :format "PNG"))
      (id:ensure-vision)
      (let* ((rgb (id::load-for-vision file))
             (cg (unwind-protect (id::cg-image-from-rgb rgb) (im:destroy rgb))))
        (unwind-protect
             (let* ((provider (id::ccall "CGImageGetDataProvider" :pointer cg :pointer))
                    (data (id::ccall "CGDataProviderCopyData" :pointer provider :pointer))
                    (bytes (id::ccall "CFDataGetBytePtr" :pointer data :pointer)))
               (is (= 40 (id::ccall "CGImageGetWidth" :pointer cg :size)))
               ;; CoreGraphics' first row is the top one: white.
               (is (= 255 (cffi:mem-aref bytes :uint8 0)))
               (is (= 0 (cffi:mem-aref bytes :uint8 (* 3 40 39))))
               (id::cf-release data))
          (id::cf-release cg))))))
