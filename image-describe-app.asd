;;;; image-describe-app.asd -- "Image Describer.app", built with asdf-macos-app.
;;;;
;;;;   make app        (or: sbcl --eval '(asdf:make "image-describe-app")' --quit)

(defsystem "image-describe-app"
  :defsystem-depends-on ("asdf-macos-app")
  :class :macos-app-system
  :build-operation "macos-app-op"
  :entry-point "image-describe:main"
  :description "Say what is in each of a folder of images, for Spotlight to find."
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :license "MIT"
  :version "0.1.0"
  :depends-on ("image-describe")
  :bundle-identifier "org.lispnik.image-describe"
  :bundle-name "Image Describer"
  :bundle-executable "image-describe"
  :bundle-icon "res/icon.png"
  :bundle-principal-class "NSApplication"
  :bundle-category "public.app-category.photography"
  :bundle-copyright "MIT"
  :bundle-log t
  ;; The folders chosen are often in one of these, and macOS wants a reason to
  ;; show the first time each is read.
  :bundle-info-plist
  (("NSDocumentsFolderUsageDescription"
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them.")
   ("NSDesktopFolderUsageDescription"
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them.")
   ("NSDownloadsFolderUsageDescription"
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them.")
   ("NSRemovableVolumesUsageDescription"
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them.")
   ("NSNetworkVolumesUsageDescription"
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them."))
  ;; Merged against this file rather than the current directory, which is what
  ;; a bare "build/" would resolve against.
  :bundle-output-directory
  #.(merge-pathnames "build/"
                     (uiop:pathname-directory-pathname
                      (or *load-truename* *default-pathname-defaults*)))
  ;; Ad hoc unless told otherwise; an empty variable counts as unset.
  :code-signing-identity #.(let ((identity (uiop:getenv "MACOS_SIGNING_IDENTITY")))
                             (if (and identity (plusp (length identity)))
                                 identity
                                 "-")))
