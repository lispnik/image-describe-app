;;;; icon.lisp -- draws res/icon.png, with IM, pixel by pixel.
;;;;
;;;;   make icon
;;;;
;;;; A picture on a teal tile, a magnifying glass over it, and a caption below
;;;; it: an image looked at, and what was seen written down.  The PNG is
;;;; checked in, so this runs only when the drawing changes; asdf-macos-app
;;;; makes the .icns.

(defpackage #:image-describe-icon
  (:use #:common-lisp)
  (:export #:main))

(in-package #:image-describe-icon)

(defparameter *size* 1024)

(defun clamp (x) (max 0 (min 255 (round x))))

(defun mix (a b f) (+ a (* (- b a) f)))

(defun coverage (distance)
  "How much of a pixel a shape covers, given the pixel's signed distance from
its edge (negative inside), with a pixel's width of soft edge."
  (max 0 (min 1 (- 0.5 distance))))

(defun rounded-rect-distance (x y left bottom right top radius)
  "The signed distance from X,Y to a rounded rectangle's edge."
  (let* ((qx (- (abs (- x (/ (+ left right) 2))) (- (/ (- right left) 2) radius)))
         (qy (- (abs (- y (/ (+ bottom top) 2))) (- (/ (- top bottom) 2) radius))))
    (- (+ (sqrt (+ (expt (max qx 0) 2) (expt (max qy 0) 2)))
          (min (max qx qy) 0))
       radius)))

(defun rounded-rect (x y left bottom right top radius)
  (coverage (rounded-rect-distance x y left bottom right top radius)))

(defun circle-distance (x y cx cy r)
  (- (sqrt (+ (expt (- x cx) 2) (expt (- y cy) 2))) r))

(defun capsule (x y ax ay bx by r)
  "Coverage of a line from A to B, R thick on each side, with round ends."
  (let* ((dx (- bx ax)) (dy (- by ay))
         (s (max 0 (min 1 (/ (+ (* (- x ax) dx) (* (- y ay) dy)) (+ (* dx dx) (* dy dy))))))
         (px (+ ax (* s dx))) (py (+ ay (* s dy))))
    (coverage (- (sqrt (+ (expt (- x px) 2) (expt (- y py) 2))) r))))

(defun landscape (u v)
  "The picture at U,V, each 0 to 1 across it: sky, a sun, two hills."
  (let ((sun (- (sqrt (+ (expt (- u 0.70) 2) (expt (- v 0.72) 2))) 0.10))
        (hill-1 (- v (+ 0.34 (* 0.20 (cos (* 5.0 (- u 0.28)))))))
        (hill-2 (- v (+ 0.20 (* 0.15 (cos (* 4.0 (- u 0.86))))))))
    (cond ((< hill-2 0) (values 46 125 72))
          ((< hill-1 0) (values 76 168 98))
          ((< sun 0) (values 255 196 61))
          (t (values (mix 120 190 v) (mix 180 225 v) 250)))))

(defparameter *glass* '(560 470 190)
  "The magnifying glass's centre and the inside radius of its rim.")

(defun shade (x y)
  "The icon at X,Y as (values r g b a); y runs upwards."
  (let ((r 0) (g 0) (b 0) (a 0))
    (flet ((over (cr cg cb amount)
             (when (plusp amount)
               (setf r (mix r cr amount) g (mix g cg amount) b (mix b cb amount)
                     a (+ a (* (- 255 a) amount))))))
      ;; The tile, inset as macOS icons are, with a vertical gradient.
      (let ((f (/ y *size*)))
        (over (mix 18 46 f) (mix 110 170 f) (mix 120 178 f)
              (rounded-rect x y 100 100 924 924 185)))
      ;; The picture, with a soft shadow and a white frame.
      (over 5 40 40 (* 0.30 (rounded-rect x y 190 318 846 798 44)))
      (over 255 255 255 (rounded-rect x y 180 330 836 810 40))
      (let ((inner (rounded-rect x y 206 356 810 784 20)))
        (when (plusp inner)
          (multiple-value-bind (pr pg pb) (landscape (/ (- x 206) 604) (/ (- y 356) 428))
            (over pr pg pb inner))))
      ;; The caption: a white card with lines of text on it.
      (over 5 40 40 (* 0.30 (rounded-rect x y 230 152 806 274 30)))
      (over 255 255 255 (rounded-rect x y 220 164 796 286 30))
      (over 70 90 110 (rounded-rect x y 262 240 640 258 9))
      (over 150 165 180 (rounded-rect x y 262 196 540 212 8))
      ;; The magnifying glass, over the picture: the glass brightens what is
      ;; under it, the rim is dark, and the handle runs down to the right.
      (destructuring-bind (cx cy radius) *glass*
        (let ((d (circle-distance x y cx cy radius)))
          (over 255 255 255 (* 0.28 (coverage d)))
          (over 20 45 60 (* (coverage (- d 34)) (- 1 (coverage d))))
          (over 20 45 60 (capsule x y (+ cx 160) (- cy 160) (+ cx 262) (- cy 262) 38))
          ;; A highlight on the glass.
          (over 255 255 255 (* 0.55 (capsule x y (- cx 110) (+ cy 60) (- cx 60) (+ cy 115) 14))))))
    (values (clamp r) (clamp g) (clamp b) (clamp a))))

(defun main (&optional (path "res/icon.png"))
  (im:with-image (image (im:create *size* *size* :color-space-rgb :data-type-byte))
    (im.ffi::%im-image-add-alpha (im:handle image))
    (let ((planes (loop for plane below 4 collect (im:plane-pointer image plane))))
      ;; IM's rows run bottom-up, which is the way y runs here too.
      (dotimes (y *size*)
        (dotimes (x *size*)
          (multiple-value-bind (r g b a) (shade (+ x 0.5) (+ y 0.5))
            (loop for pointer in planes
                  for value in (list r g b a)
                  do (setf (cffi:mem-aref pointer :uint8 (+ x (* y *size*))) value))))))
    (ensure-directories-exist path)
    (im:save image (pathname path) :format "PNG"))
  (format t "~&wrote ~a~%" path))
