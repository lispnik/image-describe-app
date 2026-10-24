;;;; package.lisp

(defpackage #:image-describe
  (:use #:common-lisp)
  (:export
   ;; property lists, as extended attributes hold them
   #:encode-binary-plist #:decode-binary-plist
   ;; extended attributes
   #:read-description #:write-description #:remove-description
   #:+comment-attribute+ #:+keywords-attribute+
   ;; scanning
   #:scan-directory #:image-file-p #:image-extensions #:relative-name #:canonical-path
   ;; identifying
   #:ensure-vision #:analyze-image #:analysis #:make-analysis
   #:analysis-labels #:analysis-text #:analysis-source
   ;; describing
   #:compose-description #:description-keywords #:humanize-label
   #:*default-minimum-confidence*
   ;; the batch
   #:entry #:make-entry #:entry-path #:entry-relative #:entry-status
   #:entry-analysis #:entry-description #:entry-saved #:entry-edited #:entry-message
   #:entry-state #:state-text #:identify-entry #:propose-description #:accept-entry
   #:entries-from-directory
   #:identification #:begin-identifying #:abandon-batch
   #:start-batch #:stop-batch #:batch-done-p #:take-finished #:wait-for-batch
   #:batch-run #:processor-count #:default-worker-count #:*worker-count*
   ;; the application
   #:main))
