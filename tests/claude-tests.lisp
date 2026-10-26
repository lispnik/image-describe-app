;;;; claude-tests.lisp -- the request to Claude, and its answers, without Anthropic.
;;;;
;;;; A server on this machine stands in for the API: it keeps the request and
;;;; gives back whatever answer the test wants, so these run without a key or
;;;; a network and cost nothing.  One test does call Anthropic, and only with
;;;; IMAGE_DESCRIBE_LIVE_CLAUDE=1 and a key.

(in-package #:image-describe/tests)

(def-suite claude :in all-tests :description "Asking Claude for a sentence.")
(in-suite claude)

(defun read-http-request (stream)
  "The headers, as an alist of lower-case names, and the body of the request on STREAM."
  (let ((headers '()))
    (loop for line = (string-right-trim '(#\Return) (read-line stream nil ""))
          until (zerop (length line))
          do (let ((colon (position #\: line)))
               (when colon
                 (push (cons (string-downcase (subseq line 0 colon))
                             (string-trim " " (subseq line (1+ colon))))
                       headers))))
    (let* ((length (parse-integer (or (cdr (assoc "content-length" headers :test #'string=)) "0")))
           (octets (make-array length :element-type '(unsigned-byte 8))))
      (read-sequence octets stream)
      (values headers (sb-ext:octets-to-string octets :external-format :utf-8)))))

(defun call-with-fake-api (status body function)
  "Call FUNCTION with *CLAUDE-ENDPOINT* a server here that answers STATUS and
BODY, JSON.  Answers the request's headers and body."
  (let* ((socket (make-instance 'sb-bsd-sockets:inet-socket :type :stream :protocol :tcp))
         (headers nil) (request nil))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket #(127 0 0 1) 0)
    (sb-bsd-sockets:socket-listen socket 1)
    (let* ((port (nth-value 1 (sb-bsd-sockets:socket-name socket)))
           (server (sb-thread:make-thread
                    (lambda () (block call-with-fake-api-server
                      (let* ((client (or (ignore-errors (sb-bsd-sockets:socket-accept socket))
                                         (return-from call-with-fake-api-server nil)))
                             (stream (sb-bsd-sockets:socket-make-stream
                                      client :input t :output t :element-type :default
                                             :external-format :latin-1 :buffering :full)))
                        (unwind-protect
                             (progn
                               (multiple-value-setq (headers request) (read-http-request stream))
                               (let ((octets (sb-ext:string-to-octets body :external-format :utf-8)))
                                 (format stream "HTTP/1.1 ~d OK~c~cContent-Type: application/json~c~cContent-Length: ~d~c~cConnection: close~c~c~c~c"
                                         status #\Return #\Newline #\Return #\Newline (length octets)
                                         #\Return #\Newline #\Return #\Newline #\Return #\Newline)
                                 (write-string (sb-ext:octets-to-string octets :external-format :latin-1) stream)
                                 (finish-output stream)))
                          (close stream)
                          (sb-bsd-sockets:socket-close client))))))))
      (unwind-protect
           (let ((id::*claude-endpoint* (format nil "http://127.0.0.1:~d/v1/messages" port)))
             (values (funcall function) headers request))
        (sb-thread:join-thread server :timeout 10 :default nil)
        (sb-bsd-sockets:socket-close socket)))))

(defun zebra-cg ()
  (id:ensure-vision)
  (id::image-io-thumbnail (account-picture "Zebra") 512))

(defmacro with-zebra ((cg) &body body)
  `(if (not (account-picture "Zebra"))
       (skip "No account pictures")
       (let ((,cg (zebra-cg)))
         (unwind-protect (progn ,@body)
           (id::cf-release ,cg)))))

(defparameter *answer*
  "{\"type\":\"message\",\"role\":\"assistant\",\"stop_reason\":\"end_turn\",
    \"content\":[{\"type\":\"text\",\"text\":\"A zebra  stands in front of a pale wall.\"}]}")

(test the-request-is-what-the-api-documents
  (with-zebra (cg)
    (multiple-value-bind (caption headers body)
        (call-with-fake-api 200 *answer* (lambda () (id::claude-caption cg 1 :key "sk-test")))
      (is (string= "A zebra stands in front of a pale wall." caption))
      (is (string= "sk-test" (cdr (assoc "x-api-key" headers :test #'string=))))
      (is (string= "2023-06-01" (cdr (assoc "anthropic-version" headers :test #'string=))))
      (is (string= "server-side-fallback-2026-07-01" (cdr (assoc "anthropic-beta" headers :test #'string=))))
      (dolist (expected '("\"model\":\"claude-opus-5-5\"" "\"fallbacks\":\"default\""
                          "\"effort\":\"low\"" "\"type\":\"image\"" "\"media_type\":\"image\\/jpeg\""
                          "\"type\":\"base64\""))
        (is (search expected body) "~a not in the request" expected)))))

(test what-goes-wrong-is-said
  (with-zebra (cg)
    (flet ((failure (status body)
             (handler-case (progn (call-with-fake-api status body
                                                      (lambda () (id::claude-caption cg 1 :key "k")))
                                  nil)
               (error (condition) (princ-to-string condition)))))
      (is (search "invalid x-api-key"
                  (failure 401 "{\"type\":\"error\",\"error\":{\"type\":\"authentication_error\",\"message\":\"invalid x-api-key\"}}")))
      (is (search "declined"
                  (failure 200 "{\"stop_reason\":\"refusal\",\"content\":[]}")))
      (is (search "no text" (failure 200 "{\"stop_reason\":\"end_turn\",\"content\":[]}")))))
  (with-zebra (cg)
    (is (search "No Anthropic API key"
                (handler-case (progn (id::claude-caption cg 1 :key nil) nil)
                  (error (condition) (princ-to-string condition)))))))

(test the-image-sent-is-small-and-upright
  (with-zebra (cg)
    (let* ((wide (id::image-io-thumbnail (account-picture "Zebra") 512))
           (base64 (id::jpeg-base64 cg 6 :largest 256))
           (data (objc:invoke (objc:invoke "NSData" "alloc") "initWithBase64EncodedString:options:"
                              base64 0))
           (source (id::ccall "CGImageSourceCreateWithData" :pointer data :pointer (cffi:null-pointer)
                              :pointer)))
      (unwind-protect
           (let ((properties (id::ccall "CGImageSourceCopyPropertiesAtIndex"
                                        :pointer source :size 0 :pointer (cffi:null-pointer) :pointer)))
             (is (equal "public.jpeg" (objc:ns-string-to-string
                                       (id::ccall "CGImageSourceGetType" :pointer source :pointer))))
             (is (<= (id::property properties "PixelWidth") 256))
             (is (<= (id::property properties "PixelHeight") 256))
             (id::cf-release properties))
        (id::cf-release source)
        (id::cf-release wide)
        (objc:release data)))))

(test the-key-is-kept-in-the-keychain
  (if (not (live-p "IMAGE_DESCRIBE_LIVE_KEYCHAIN"))
      (skip "Set IMAGE_DESCRIBE_LIVE_KEYCHAIN=1 to write to the login Keychain")
      (let ((before (id::keychain-api-key)))
        (unwind-protect
             (progn (id::store-api-key "sk-ant-test-only")
                    (is (string= "sk-ant-test-only" (id::keychain-api-key)))
                    (id::store-api-key "")
                    (is (null (id::keychain-api-key))))
          (when before (id::store-api-key before))))))

(test claude-describes-a-zebra
  (cond ((not (live-p "IMAGE_DESCRIBE_LIVE_CLAUDE"))
         (skip "Set IMAGE_DESCRIBE_LIVE_CLAUDE=1, with a key, to ask Anthropic (about a cent)"))
        ((null (id::api-key)) (skip "No Anthropic API key"))
        (t (with-zebra (cg)
             (let ((caption (id::claude-caption cg 1)))
               (is (search "zebra" caption :test #'char-equal) "~a" caption))))))
