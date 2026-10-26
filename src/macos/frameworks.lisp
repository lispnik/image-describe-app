;;;; frameworks.lisp -- AppKit: loading it, its constants, and the log.
;;;;
;;;; Nothing foreign is made when this is loaded.  The application is a saved
;;;; image, and a pointer made in the build would be garbage in the run.

(in-package #:image-describe)

(defparameter +cocoa-framework+
  "/System/Library/Frameworks/Cocoa.framework/Versions/A/Cocoa")

(defun ensure-frameworks ()
  "Bring up the Objective-C runtime with AppKit and Vision in it.  Every entry
point that touches AppKit calls this first; in the bundle it is also what
rebuilds the classes defined here."
  (objc:ensure-objc-initialized :modules (list +cocoa-framework+))
  (ensure-vision))

;;; Constants -------------------------------------------------------------------

(defconstant +ns-window-style-mask+ 15
  "Titled, closable, miniaturizable, resizable.")
(defconstant +ns-backing-store-buffered+ 2)

;;; Autoresizing: which of a view's margins and dimensions give when its
;;; superview changes size.  AppKit's y runs upwards, so "min y" is the bottom.
(defconstant +flexible-left+ 1)
(defconstant +flexible-width+ 2)
(defconstant +flexible-right+ 4)
(defconstant +flexible-bottom+ 8)
(defconstant +flexible-height+ 16)
(defconstant +flexible-top+ 32)

(defconstant +ns-modal-response-ok+ 1)
(defconstant +ns-bezel-border+ 2)
(defconstant +ns-table-view-style-full-width+ 1)
(defconstant +ns-table-view-last-column-only-autoresizing-style+ 4)
(defconstant +ns-alert-first-button-return+ 1000)
(defconstant +ns-line-break-by-word-wrapping+ 0)
(defconstant +ns-line-break-by-truncating-tail+ 4)
(defconstant +ns-line-break-by-truncating-middle+ 5)
(defconstant +ns-image-frame-gray-bezel+ 2)
(defconstant +ns-image-scale-proportionally-down+ 0)
(defconstant +ns-terminate-cancel+ 0)
(defconstant +ns-terminate-now+ 1)

(defparameter +run-loop-modes+
  #("NSDefaultRunLoopMode" "NSEventTrackingRunLoopMode" "NSModalPanelRunLoopMode")
  "The modes the next look at the workers is queued for: the usual one, and
the ones that run while the window is resized or a panel is up, so that
choosing a folder does not stop the rest being identified.")

;;; Small things ------------------------------------------------------------------
