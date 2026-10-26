;;;; claude.lisp -- a sentence about the image, from Claude, when asked for.
;;;;
;;;; Vision names things; it does not say "a bald eagle in profile against a
;;;; black background".  With this on, each image is also sent -- as a JPEG
;;;; no larger than 1024 pixels -- to Anthropic's Messages API, and the
;;;; sentence that comes back leads the proposed description.  It is off
;;;; unless chosen, because the image leaves the Mac and each one costs money.
;;;;
;;;; Common Lisp has no Anthropic SDK, so this is the documented HTTP API,
;;;; sent with NSURLConnection's synchronous call from the worker's own
;;;; thread: no completion block, so no Lisp running on a thread SBCL did not
;;;; make.  The key is the ANTHROPIC_API_KEY environment variable if set,
;;;; else one kept in the login Keychain (STORE-API-KEY).

(in-package #:image-describe)

(defparameter +security-framework+
  "/System/Library/Frameworks/Security.framework/Versions/A/Security")

(defparameter *claude-model* "claude-opus-5-5")

(defparameter *claude-endpoint* "https://api.anthropic.com/v1/messages")

(defparameter *caption-prompt*
  "Describe this image in one plain sentence, for finding it again later by
searching: say what it shows -- the main subjects, the setting, anything
notable. No preamble, no quotation marks, no more than 30 words.")

(defparameter *caption-largest-side* 1024
  "The longest side of the JPEG sent: enough to see what is in a picture,
and about 1,400 input tokens.")

;;; The key -------------------------------------------------------------------------

(defparameter +keychain-service+ "org.lispnik.image-describe")
(defparameter +keychain-account+ "anthropic-api-key")

(defun ensure-security ()
  (objc:ensure-objc-initialized :modules (list +security-framework+)))

(defun keychain-query (&rest more)
  (ensure-security)
  (let ((query (apply #'dictionary
                      "kSecAttrService" +keychain-service+
                      "kSecAttrAccount" +keychain-account+
                      more)))
    (objc:invoke query "setObject:forKey:" (cf-key "kSecClassGenericPassword") (cf-key "kSecClass"))
    query))

(defun keychain-api-key ()
  "The key kept in the Keychain, or NIL."
  (cffi:with-foreign-object (result :pointer)
    (let ((query (keychain-query)))
      (objc:invoke query "setObject:forKey:" (ns-number t) (cf-key "kSecReturnData"))
      (when (zerop (ccall "SecItemCopyMatching" :pointer query :pointer result :int))
        (let ((data (cffi:mem-ref result :pointer)))
          (unwind-protect
               (let ((text (objc:invoke (objc:invoke "NSString" "alloc") "initWithData:encoding:"
                                        data 4))) ; NSUTF8StringEncoding
                 (prog1 (objc:ns-string-to-string text)
                   (objc:release text)))
            (cf-release data)))))))

(defun store-api-key (key)
  "Keep KEY in the login Keychain, replacing any kept before; a blank KEY
removes it.  Signals if the Keychain refuses."
  (ccall "SecItemDelete" :pointer (keychain-query) :int)
  (let ((key (string-trim '(#\Space #\Tab #\Newline #\Return) (or key ""))))
    (when (plusp (length key))
      (let* ((query (keychain-query))
             (bytes (sb-ext:string-to-octets key :external-format :utf-8))
             (data (cffi:with-pointer-to-vector-data (pointer bytes)
                     (objc:invoke "NSData" "dataWithBytes:length:" pointer (length bytes)))))
        (objc:invoke query "setObject:forKey:" data (cf-key "kSecValueData"))
        (let ((status (ccall "SecItemAdd" :pointer query :pointer (cffi:null-pointer) :int)))
          (unless (zerop status)
            (error "The Keychain would not keep the key (error ~d)." status)))))
    (plusp (length key))))

(defun api-key ()
  "The key to use, or NIL: ANTHROPIC_API_KEY, else the Keychain's."
  (let ((env (uiop:getenv "ANTHROPIC_API_KEY")))
    (if (and env (plusp (length env)))
        env
        (ignore-errors (keychain-api-key)))))

;;; The request ---------------------------------------------------------------------

(defun jpeg-base64 (cg orientation &key (largest *caption-largest-side*))
  "CG, a CGImage with EXIF ORIENTATION, as a base64 JPEG no larger than
LARGEST, upright: Claude does not read the orientation tag."
  (let ((data (objc:invoke "NSMutableData" "data")))
    (let ((destination (ccall "CGImageDestinationCreateWithData"
                              :pointer data :pointer (ns-string "public.jpeg") :size 1
                              :pointer (cffi:null-pointer) :pointer)))
      (when (cffi:null-pointer-p destination)
        (error "ImageIO could not make a JPEG."))
      (unwind-protect
           (progn
             (ccall "CGImageDestinationAddImage"
                    :pointer destination :pointer (upright cg orientation)
                    :pointer (dictionary "kCGImageDestinationImageMaxPixelSize" largest
                                         "kCGImageDestinationLossyCompressionQuality" 0.8d0)
                    :void)
             (unless (ccall "CGImageDestinationFinalize" :pointer destination :bool)
               (error "ImageIO could not write the JPEG.")))
        (cf-release destination)))
    (objc:invoke-into 'string data "base64EncodedStringWithOptions:" 0)))

(defun upright (cg orientation)
  "CG turned as EXIF ORIENTATION says, autoreleased; or CG itself if it is
upright already."
  (if (eql orientation 1)
      cg
      (let* ((ci (objc:invoke (objc:invoke "CIImage" "imageWithCGImage:" cg)
                              "imageByApplyingCGOrientation:" orientation))
             (context (objc:invoke "CIContext" "contextWithOptions:" nil))
             (turned (objc:invoke context "createCGImage:fromRect:" ci (objc:invoke ci "extent"))))
        (if (null-object-p turned)
            cg
            (ccall "CFAutorelease" :pointer turned :pointer)))))

(defun json-data (object)
  (objc:invoke-with-error "NSJSONSerialization" "dataWithJSONObject:options:error:" object 0))

(defun request-body (image-base64)
  (dictionary
   "model" *claude-model*
   "max_tokens" 1024
   ;; A one-sentence caption is simple work.
   "output_config" (dictionary "effort" "low")
   ;; A request a safety classifier declines is run again on a model
   ;; chosen for why it was declined, inside the same call.
   "fallbacks" "default"
   "messages"
   (objc:invoke "NSArray" "arrayWithObject:"
                (dictionary
                 "role" "user"
                 "content"
                 ;; A vector is an NSArray to the bridge.
                 (vector (dictionary "type" "image"
                                     "source" (dictionary "type" "base64"
                                                          "media_type" "image/jpeg"
                                                          "data" image-base64))
                         (dictionary "type" "text" "text" *caption-prompt*))))))

(defun post-json (body key)
  "POST BODY, an NSDictionary, to the Messages API with KEY.  Answers the
response's HTTP status and its parsed JSON."
  (let ((request (objc:invoke "NSMutableURLRequest" "requestWithURL:cachePolicy:timeoutInterval:"
                              (objc:invoke "NSURL" "URLWithString:" *claude-endpoint*)
                              1 120d0))) ; ignoring the cache, two minutes
    (objc:invoke request "setHTTPMethod:" "POST")
    (loop for (header value) in `(("content-type" "application/json")
                                  ("x-api-key" ,key)
                                  ("anthropic-version" "2023-06-01")
                                  ("anthropic-beta" "server-side-fallback-2026-07-01"))
          do (objc:invoke request "setValue:forHTTPHeaderField:" value header))
    (objc:invoke request "setHTTPBody:" (json-data body))
    (cffi:with-foreign-object (response :pointer)
      (setf (cffi:mem-ref response :pointer) (cffi:null-pointer))
      (let ((data (objc:invoke-with-error "NSURLConnection"
                                          "sendSynchronousRequest:returningResponse:error:"
                                          request response)))
        (values (let ((http (cffi:mem-ref response :pointer)))
                  (if (cffi:null-pointer-p http) 0 (objc:invoke http "statusCode")))
                (and (not (null-object-p data))
                     (ignore-errors
                      (objc:invoke-with-error "NSJSONSerialization"
                                              "JSONObjectWithData:options:error:" data 0))))))))

(defun response-text (json)
  "The text blocks of a Messages API response, joined."
  (let ((content (objc:invoke json "objectForKey:" (ns-string "content")))
        (parts '()))
    (unless (null-object-p content)
      (dotimes (i (objc:invoke content "count"))
        (let ((block (objc:invoke content "objectAtIndex:" i)))
          (when (equal "text" (property block "type"))
            (push (property block "text") parts)))))
    (string-trim '(#\Space #\Newline #\")
                 (format nil "~{~a~^ ~}" (nreverse (remove nil parts))))))

(defun claude-caption (cg orientation &key (key (api-key)))
  "A sentence about CG, a CGImage with EXIF ORIENTATION, from Claude.  For
*CAPTION-FUNCTION*.  Signals why not."
  (unless key
    (error "No Anthropic API key: set one with Describe with Claude… Key."))
  (objc:with-autorelease-pool ()
    (multiple-value-bind (status json) (post-json (request-body (jpeg-base64 cg orientation)) key)
      (cond ((null json)
             (error "Claude did not answer (HTTP ~d)." status))
            ((/= status 200)
             (error "Claude said: ~a (HTTP ~d)."
                    (or (property json "error" "message") "an error") status))
            ((equal "refusal" (property json "stop_reason"))
             (error "Claude declined to describe this image."))
            (t
             (let ((text (collapse-spaces (response-text json))))
               (if (plusp (length text))
                   text
                   (error "Claude's answer had no text."))))))))
