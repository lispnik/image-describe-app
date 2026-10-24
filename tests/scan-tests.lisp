;;;; scan-tests.lisp -- finding the images in a folder.

(in-package #:image-describe/tests)

(def-suite scan :in all-tests :description "Finding images.")
(in-suite scan)

(test images-are-found-by-extension
  (with-temporary-directory (dir)
    (dolist (name '("b.png" "a.JPG" "c.heic" "d.webp" "e.pcx" "notes.txt" "noextension"
                    ".hidden.png" "sub/f.tif" ".git/g.png" "Photos Library.photoslibrary/h.jpg"))
      (write-text (path dir name) ""))
    (is (equal '("a.JPG" "b.png" "c.heic" "d.webp" "e.pcx")
               (names (id:scan-directory dir) dir)))
    (is (equal '("a.JPG" "b.png" "c.heic" "d.webp" "e.pcx" "sub/f.tif")
               (names (id:scan-directory dir :recursive t) dir)))))

(test the-paths-are-canonical
  ;; /tmp is a link to /private/tmp.
  (with-temporary-directory (dir)
    (write-text (path dir "a.png") "")
    (let ((found (first (id:scan-directory dir))))
      (is (string= (path (id:canonical-path dir) "a.png") found))
      (is (string= "a.png" (id:relative-name found dir))))))

(test a-link-back-up-is-not-followed
  (with-temporary-directory (dir)
    (write-text (path dir "sub" "a.png") "")
    (sb-posix:symlink dir (path dir "sub" "loop"))
    (is (equal '("sub/a.png") (names (id:scan-directory dir :recursive t) dir)))))

(test no-folder-no-images
  (is (null (id:scan-directory "/no/such/folder" :recursive t))))

(test the-common-formats-are-images
  (dolist (extension '("jpg" "jpeg" "png" "tif" "tiff" "gif" "bmp" "heic" "webp" "pcx" "tga"))
    (is-true (id:image-file-p (format nil "/x/photo.~a" extension)) extension))
  (is-false (id:image-file-p "/x/photo.txt"))
  (is-false (id:image-file-p "/x/photo")))
