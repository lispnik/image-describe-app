;;;; batch-tests.lisp -- the rows, their states, and the worker pool.

(in-package #:image-describe/tests)

(def-suite batch :in all-tests :description "Entries and the batch.")
(in-suite batch)

(defun identified (labels &rest initargs)
  (apply #'id:make-entry :status :identified
                         :analysis (id:make-analysis :labels labels)
                         initargs))

(test the-states-of-an-entry
  (is (null (id:entry-state (id:make-entry))))
  (is (eq :identifying (id:entry-state (id:make-entry :status :identifying))))
  (is (eq :error (id:entry-state (id:make-entry :status :error :message "no"))))
  (is (eq :saved (id:entry-state (id:make-entry :description "Dog" :saved "Dog"))))
  (is (eq :proposed (id:entry-state (identified '() :description "Dog"))))
  (is (eq :edited (id:entry-state (identified '() :description "Dog" :saved "Cat" :edited t))))
  (is (eq :nothing (id:entry-state (identified '() :description ""))))
  (is (string= "to review" (id:state-text :proposed)))
  (is (string= "" (id:state-text nil))))

(test a-proposal-never-replaces-what-was-typed
  (let ((entry (identified '(("dog" . 0.9d0)))))
    (is-true (id:propose-description entry))
    (is (string= "Dog" (id:entry-description entry)))
    (setf (id:entry-description entry) "Rex" (id:entry-edited entry) t)
    (is-false (id:propose-description entry :minimum-confidence 0.1))
    (is (string= "Rex" (id:entry-description entry)))))

(test identifying-records-an-error-and-never-signals
  (with-temporary-directory (dir)
    (let ((entry (id:make-entry :path (write-text (path dir "bad.png") "no"))))
      (finishes (id:identify-entry entry))
      (is (eq :error (id:entry-status entry)))
      (is (plusp (length (id:entry-message entry)))))))

(test accepting-writes-the-file
  (with-temporary-directory (dir)
    (let ((entry (identified '(("dog" . 0.9d0)) :path (write-text (path dir "a.png") ""))))
      (id:propose-description entry)
      (is-true (id:accept-entry entry))
      (is (eq :saved (id:entry-state entry)))
      (is (string= "Dog" (id:read-description (path dir "a.png"))))
      ;; And the folder, listed again, says so.
      (let ((listed (aref (id:entries-from-directory dir) 0)))
        (is (string= "Dog" (id:entry-saved listed)))
        (is (eq :saved (id:entry-state listed))))
      ;; Accepting nothing takes it off.
      (setf (id:entry-description entry) "" (id:entry-edited entry) t)
      (is-true (id:accept-entry entry))
      (is (null (id:read-description (path dir "a.png")))))))

(test a-file-that-cannot-take-it-is-an-error
  (with-temporary-directory (dir)
    (let ((entry (identified '(("dog" . 0.9d0)) :path (write-text (path dir "a.png") ""))))
      (id:propose-description entry)
      (sb-posix:chmod (id:entry-path entry) #o444)
      (is-false (id:accept-entry entry))
      (is (eq :error (id:entry-state entry)))
      (is (search "Permission denied" (id:entry-message entry)))
      ;; Once it can, the error goes.
      (sb-posix:chmod (id:entry-path entry) #o644)
      (is-true (id:accept-entry entry))
      (is (eq :saved (id:entry-state entry))))))

;;; The worker pool -----------------------------------------------------------------

(test a-batch-on-several-threads-does-each-once
  (let* ((entries (coerce (loop for i below 50 collect (id:make-entry :relative (princ-to-string i)))
                          'vector))
         (seen (make-hash-table))
         (lock (sb-thread:make-mutex))
         (calls 0)
         (run (id:start-batch entries
                              (lambda (entry)
                                (sb-thread:with-mutex (lock) (incf (gethash entry seen 0)))
                                (sleep 0.001)
                                (when (string= "13" (id:entry-relative entry))
                                  (error "This one fails."))
                                (setf (id:entry-status entry) :identified))
                              :workers 6
                              :on-progress (lambda (run)
                                             (declare (ignore run))
                                             (sb-thread:with-mutex (lock) (incf calls))))))
    (is-true (id:wait-for-batch run :timeout 60))
    (is-true (id:batch-done-p run))
    (is (equal (loop for i below 50 collect i) (sort (id:take-finished run) #'<)))
    (is (null (id:take-finished run)))
    (is (every (lambda (entry) (= 1 (gethash entry seen 0))) entries))
    ;; One call an entry, and one as each worker ended.
    (is (= (+ 50 6) calls))
    ;; An error nothing foresaw still answers the row.
    (is (eq :error (id:entry-status (aref entries 13))))
    (is (search "fails" (id:entry-message (aref entries 13))))))

(test only-the-indices-asked-for
  (let* ((entries (coerce (loop repeat 10 collect (id:make-entry)) 'vector))
         (run (id:start-batch entries (lambda (entry) (setf (id:entry-status entry) :identified))
                              :indices '(2 5 7) :workers 4)))
    (id:wait-for-batch run :timeout 30)
    (is (equal '(2 5 7) (sort (id:take-finished run) #'<)))
    (is (= 3 (count :identified entries :key #'id:entry-status)))))

(test stopping-hands-out-no-more
  (let* ((entries (coerce (loop repeat 40 collect (id:make-entry)) 'vector))
         (run (id:start-batch entries (lambda (entry)
                                        (sleep 0.02)
                                        (setf (id:entry-status entry) :identified))
                              :workers 2)))
    (sleep 0.05)
    (id:stop-batch run)
    (is-true (id:wait-for-batch run :timeout 30))
    (let ((done (count :identified entries :key #'id:entry-status)))
      (is (< 0 done 40) "~d done" done))))

(test an-empty-batch-is-done-at-once
  (let ((run (id:start-batch #() #'identity)))
    (is-true (id:batch-done-p run))
    (is-true (id:wait-for-batch run :timeout 1))))

(test identifying-real-images-on-several-threads
  (with-temporary-directory (dir)
    (dotimes (i 12)
      (make-sample (path dir (format nil "~2,'0d.png" i)) :kind (nth (mod i 3) '(:rgb :gray :map))))
    (let ((zebra (account-picture "Zebra")))
      (when zebra (copy-file zebra (path dir "zebra.heic"))))
    (write-text (path dir "zz-broken.png") "no")
    (let* ((entries (id:entries-from-directory dir))
           (run (id:start-batch entries (lambda (entry) (id:identify-entry entry)) :workers 4)))
      (is-true (id:wait-for-batch run :timeout 120))
      (loop for entry across entries
            do (if (string= "zz-broken.png" (id:entry-relative entry))
                   (is (eq :error (id:entry-status entry)))
                   (is (eq :identified (id:entry-status entry)) "~a: ~a"
                       (id:entry-relative entry) (id:entry-message entry))))
      (let ((zebra (find "zebra.heic" entries :key #'id:entry-relative :test #'string=)))
        (when zebra
          (id:propose-description zebra)
          (is (search "Zebra" (id:entry-description zebra))))))))

(test an-abandoned-run-records-nothing-more
  (let* ((entries (coerce (loop repeat 6 collect (id:make-entry)) 'vector))
         (release (sb-thread:make-semaphore))
         (run (id:start-batch entries
                              (lambda (entry)
                                ;; Stuck, as on a slow disk, until released.
                                (sb-thread:wait-on-semaphore release)
                                (lambda () (setf (id:entry-status entry) :identified)))
                              :begin #'id:begin-identifying
                              :workers 2)))
    (loop repeat 200 until (= 2 (count :identifying entries :key #'id:entry-status))
          do (sleep 0.01))
    (is (= 2 (count :identifying entries :key #'id:entry-status)))
    (is-false (id:batch-done-p run))
    (let ((left (id:abandon-batch run)))
      (is (= 2 (length left)))
      (is-true (id:batch-done-p run))
      (dolist (index left) (setf (id:entry-status (aref entries index)) nil))
      (sb-thread:signal-semaphore release 6)
      (is-true (id:wait-for-batch run :timeout 10))
      ;; What the stuck workers found is thrown away, and none started another.
      (is (every #'null (map 'list #'id:entry-status entries)))
      (is (null (id:take-finished run))))))

(test the-file-is-read-ahead-before-im-reads-it
  (with-temporary-directory (dir)
    (let ((file (make-sample (path dir "a.gif") :kind :map :format "GIF")))
      (finishes (id::read-ahead file))
      (finishes (id::read-ahead (path dir "missing.gif")))
      (im:destroy (id::load-for-vision file)))))
