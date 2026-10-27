;;;; staged.lisp -- the window mid-review, on a backdrop, for the README.
;;;;
;;;;   make staged
;;;;
;;;; Makes a folder of pictures in build/Staged Pictures -- macOS's account
;;;; pictures under friendlier names, a photograph that records when and
;;;; where it was taken, a trail sign, a QR code -- and identifies it in the
;;;; real window.  Then, as someone part way through would have: three are
;;;; accepted, one is retyped, and the photograph is selected, its place name
;;;; looked up from Apple.  AppKit draws the window, which is put on a
;;;; gradient with rounded corners and a shadow in doc/staged.png.
;;;;
;;;; Only the copies in build/ are written to.

(defpackage #:image-describe-staged
  (:use #:common-lisp)
  (:local-nicknames (#:id #:image-describe) (#:tests #:image-describe/tests))
  (:export #:main))

(in-package #:image-describe-staged)

(defparameter *folder* "build/Staged Pictures")

;; (name account-picture) for the plain copies.
(defparameter *pictures*
  '(("Bald eagle" "Eagle") ("Owl at dusk" "Owl") ("Parrot" "Parrot")
    ("Penguin" "Penguin") ("Zebra" "Zebra") ("Sunflower" "Sunflower")
    ("Red rose" "Red Rose") ("Dahlia" "Dahlia") ("Violin" "Violin")))

(defparameter *accepted* '("Bald eagle" "Owl at dusk" "Parrot"))

(defparameter *retyped* '("Zebra" . "Zebra, mammal, animal. Grandma's favourite at the zoo"))

(defparameter *selected* "Storm over Half Dome")

(defun settle (&optional (seconds 0.3d0))
  (objc:invoke (objc:invoke "NSRunLoop" "currentRunLoop") "runUntilDate:"
               (objc:invoke "NSDate" "dateWithTimeIntervalSinceNow:" seconds)))

(defun picture (name)
  "The path of macOS's account picture NAME, in whichever of its folders."
  (let ((found (directory (format nil "/Library/User Pictures/*/~a.heic" name))))
    (and found (id::native (first found)))))

(defun file (name type)
  (format nil "~a~a/~a.~a" (id::native (uiop:getcwd)) *folder* name type))

(defun make-folder ()
  (let ((directory (sb-ext:parse-native-namestring (format nil "~a~a/" (id::native (uiop:getcwd)) *folder*))))
    (when (uiop:directory-exists-p directory)
      (uiop:delete-directory-tree directory :validate t))
    (ensure-directories-exist directory))
  (loop for (name picture) in *pictures*
        for from = (picture picture)
        when from do (tests::copy-file from (file name "heic")))
  (tests::make-photo (file *selected* "jpg")
                     :from (picture "Lightning")
                     :date "2023:07:04 18:22:01" :latitude 37.7459d0 :longitude -119.5332d0)
  (tests::text-image "MIST TRAIL  2.4 MI" (file "Trail sign" "png"))
  (tests::make-qr-code (file "Trail map QR" "png") "https://www.nps.gov/yose/"))

(defun row-named (controller name)
  (position-if (lambda (entry)
                 (string= name (pathname-name (sb-ext:parse-native-namestring (id::entry-path entry)))))
               (id::controller-entries controller)))

(defun stage (controller)
  "Identify the folder, then accept, retype and select as a person would."
  (objc:invoke (id::controller-folder-field controller) "setStringValue:"
               (string-right-trim "/." (file "" "")))
  (id::rescan controller)
  (id::start-identifying controller)
  (loop while (id::collect-progress controller) do (sleep 0.05))
  (objc:invoke (id::controller-places-checkbox controller) "setState:" 1)
  (id::settings-changed controller)
  (loop repeat 200 while (id::place-lookups-pending-p) do (settle 0.1d0))
  (id::settings-changed controller)
  (id::accept-rows controller (mapcar (lambda (name) (row-named controller name)) *accepted*))
  (let ((entry (aref (id::controller-entries controller) (row-named controller (car *retyped*)))))
    (setf (id::entry-description entry) (cdr *retyped*)
          (id::entry-edited entry) t))
  (objc:invoke (id::controller-table controller) "reloadData")
  (let ((row (row-named controller *selected*)))
    (id::select-row controller row)
    ;; Every row fits; start from the top, not scrolled to the selection.
    (objc:invoke (id::controller-table controller) "scrollRowToVisible:" 0)
    (id::show-selection controller)
    (id::refresh-controls controller)
    (loop repeat 200
          until (let ((image (objc:invoke (id::controller-preview controller) "image")))
                  (not (or (null image) (cffi:null-pointer-p image))))
          do (settle 0.05d0)
             (objc:invoke (objc:objc-object-pointer controller) "previewReady:" nil)))
  (objc:invoke (id::controller-window controller) "makeFirstResponder:"
               (id::controller-table controller))
  ;; Shown as a folder of someone's, not as build/.
  (objc:invoke (id::controller-folder-field controller) "setStringValue:"
               "~/Pictures/Yosemite Trip"))

(defun window-rep (window)
  "The window, frame and all, drawn into a bitmap at the screen's scale."
  (objc:invoke window "displayIfNeeded")
  (settle)
  (let* ((view (objc:invoke (objc:invoke window "contentView") "superview"))
         (bounds (objc:invoke view "bounds"))
         (rep (objc:invoke view "bitmapImageRepForCachingDisplayInRect:" bounds)))
    (objc:invoke view "cacheDisplayInRect:toBitmapImageRep:" bounds rep)
    rep))

(defun color (r g b &optional (a 1d0))
  (objc:invoke "NSColor" "colorWithSRGBRed:green:blue:alpha:" r g b a))

(defun backdrop (rep path &key (margin 140) (radius 32d0))
  "REP on a gradient, its corners rounded and a shadow under it, as a PNG at PATH."
  (let* ((w (objc:invoke rep "pixelsWide"))
         (h (objc:invoke rep "pixelsHigh"))
         (cw (+ w (* 2 margin)))
         (ch (+ h (* 2 margin)))
         (canvas (objc:invoke (objc:invoke "NSBitmapImageRep" "alloc")
                              "initWithBitmapDataPlanes:pixelsWide:pixelsHigh:bitsPerSample:samplesPerPixel:hasAlpha:isPlanar:colorSpaceName:bytesPerRow:bitsPerPixel:"
                              (cffi:null-pointer) cw ch 8 4 t nil "NSCalibratedRGBColorSpace" 0 0))
         (frame (vector (float margin 1d0) (float margin 1d0) (float w 1d0) (float h 1d0)))
         (outline (objc:invoke "NSBezierPath" "bezierPathWithRoundedRect:xRadius:yRadius:"
                               frame radius radius)))
    (objc:invoke canvas "setSize:" (vector (float cw 1d0) (float ch 1d0)))
    (objc:invoke "NSGraphicsContext" "saveGraphicsState")
    (objc:invoke "NSGraphicsContext" "setCurrentContext:"
                 (objc:invoke "NSGraphicsContext" "graphicsContextWithBitmapImageRep:" canvas))
    ;; The icon's teal, into a deep blue.
    (let ((gradient (objc:invoke (objc:invoke "NSGradient" "alloc") "initWithStartingColor:endingColor:"
                                 (color 0.20d0 0.62d0 0.64d0) (color 0.16d0 0.22d0 0.48d0))))
      (objc:invoke gradient "drawInRect:angle:" (vector 0d0 0d0 (float cw 1d0) (float ch 1d0)) -55d0)
      (objc:release gradient))
    (let ((shadow (objc:invoke (objc:invoke "NSShadow" "alloc") "init")))
      (objc:invoke shadow "setShadowColor:" (color 0d0 0d0 0d0 0.45d0))
      (objc:invoke shadow "setShadowOffset:" #(0d0 -24d0))
      (objc:invoke shadow "setShadowBlurRadius:" 60d0)
      (objc:invoke "NSGraphicsContext" "saveGraphicsState")
      (objc:invoke shadow "set")
      (objc:invoke (color 1d0 1d0 1d0) "set")
      (objc:invoke outline "fill")
      (objc:invoke "NSGraphicsContext" "restoreGraphicsState")
      (objc:release shadow))
    (objc:invoke "NSGraphicsContext" "saveGraphicsState")
    (objc:invoke outline "addClip")
    (objc:invoke rep "drawInRect:" frame)
    (objc:invoke "NSGraphicsContext" "restoreGraphicsState")
    (objc:invoke (color 0d0 0d0 0d0 0.18d0) "set")
    (objc:invoke outline "setLineWidth:" 2d0)
    (objc:invoke outline "stroke")
    (objc:invoke "NSGraphicsContext" "restoreGraphicsState")
    (let ((data (objc:invoke canvas "representationUsingType:properties:" 4
                             (objc:invoke "NSDictionary" "dictionary"))))
      (ensure-directories-exist path)
      (objc:invoke data "writeToFile:atomically:" (namestring path) t))
    (objc:release canvas)
    (format t "~&wrote ~a~%" path)))

(defun main ()
  (let ((id::*remember-settings* nil)
        (id::*list-in-background* nil))
    (id::ensure-frameworks)
    (objc:with-autorelease-pool ()
      (objc.runloop:shared-application :activation-policy nil)
      (objc:invoke (objc.runloop:shared-application) "setAppearance:"
                   (objc:invoke "NSAppearance" "appearanceNamed:" "NSAppearanceNameAqua"))
      (make-folder)
      (let* ((controller (id::make-window-controller))
             (window (id::controller-window controller)))
        (objc:invoke window "orderFront:" nil)
        (stage controller)
        (backdrop (window-rep window) #p"doc/staged.png")))))
