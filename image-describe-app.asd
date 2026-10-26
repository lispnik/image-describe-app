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
  ;; SBCL's three, and Apple Events for Show in Get Info.
  :entitlements #.(merge-pathnames "res/entitlements.plist"
                                   (uiop:pathname-directory-pathname
                                    (or *load-truename* *default-pathname-defaults*)))
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
    . "Image Describer reads the images in the folders you choose and writes their descriptions onto them.")
   ;; Show in Get Info: Finder sets the comments it shows there.
   ("NSAppleEventsUsageDescription"
    . "Image Describer asks Finder to show each description as the image's Get Info comment, when you choose that.")
   ;; Open With, and dropping onto the Dock icon.
   ("CFBundleDocumentTypes"
    . (:array (:dict ("CFBundleTypeName" . "Folder")
                     ("CFBundleTypeRole" . "Viewer")
                     ("LSHandlerRank" . "None")
                     ("LSItemContentTypes" . (:array "public.folder")))
              (:dict ("CFBundleTypeName" . "Image")
                     ("CFBundleTypeRole" . "Viewer")
                     ("LSHandlerRank" . "Alternate")
                     ("LSItemContentTypes" . (:array "public.image")))))
   ;; The Finder Quick Action.
   ("NSServices"
    . (:array (:dict ("NSMenuItem" . (:dict ("default" . "Describe with Image Describer")))
                     ("NSMessage" . "describeFiles")
                     ("NSPortName" . "Image Describer")
                     ("NSRequiredContext" . (:dict))
                     ("NSSendFileTypes" . (:array "public.image" "public.folder"))))))
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
