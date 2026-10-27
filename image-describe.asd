;;;; image-describe.asd -- identifying images, their window, and the tests.
;;;;
;;;; The bundle is image-describe-app.asd.  It is a file of its own because
;;;; :DEFSYSTEM-DEPENDS-ON is resolved when a .asd is read, and loading the
;;;; describer should not need asdf-macos-app.

(defsystem "image-describe/core"
  :description "What is in an image, from Vision, kept on the file for Spotlight; no window."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("im" "cffi" "objc")
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "plist")
                             (:file "vision")
                             (:file "describe")
                             (:file "xattr")
                             (:file "scan")
                             (:file "places")
                             (:file "finder")
                             (:file "batch")
                             (:file "cli"))))
  :in-order-to ((test-op (test-op "image-describe/tests"))))

(defsystem "image-describe"
  :description "Describe a folder of images for Spotlight, in a Cocoa window."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("image-describe/core" (:require "sb-posix"))
  :components ((:module "src/macos"
                :pathname "src/macos/"
                :serial t
                :components ((:file "frameworks")
                             (:file "preview")
                             (:file "window")
                             (:file "app"))))
  :in-order-to ((test-op (test-op "image-describe/tests"))))

(defsystem "image-describe/tests"
  :description "The FiveAM suite."
  :depends-on ("image-describe" "fiveam" (:require "sb-posix"))
  :components ((:module "tests"
                :serial t
                :components ((:file "package")
                             (:file "plist-tests")
                             (:file "describe-tests")
                             (:file "xattr-tests")
                             (:file "scan-tests")
                             (:file "vision-tests")
                             (:file "batch-tests")
                             (:file "cli-tests")
                             (:file "ui-tests"))))
  ;; FIVEAM:RUN! prints failures but returns NIL, and ASDF discards what a
  ;; TEST-OP returns, so the failure has to be an error.
  :perform (test-op (o c)
             (unless (uiop:symbol-call :image-describe/tests :run-tests)
               (error "The test suite failed."))))
