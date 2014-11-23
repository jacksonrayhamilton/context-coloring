(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-test-resolve-path (path)
  (expand-file-name path context-coloring-path))

;; Load expected output constants.
(load-file (context-coloring-test-resolve-path "./fixtures/scopes.el"))
(load-file (context-coloring-test-resolve-path "./fixtures/nested.el"))
(load-file (context-coloring-test-resolve-path "./fixtures/vow.el"))

(defun get-string-from-file (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun context-coloring-test-read-file (path)
  (get-string-from-file (context-coloring-test-resolve-path path)))

(defmacro context-coloring-test-with-fixture (fixture &rest body)
  "Evaluate BODY in a temporary buffer with the relative
FIXTURE."
  `(with-temp-buffer
     (insert (context-coloring-test-read-file ,fixture))
     (context-coloring-mode)
     ,@body))

(ert-deftest context-coloring-test-scopes ()
  (context-coloring-test-with-fixture "./fixtures/scopes.js"
   (should (equal (buffer-substring (point-min) (point-max))
                   context-coloring-test-expected-scopes))))

(ert-deftest context-coloring-test-nested ()
  (context-coloring-test-with-fixture "./fixtures/nested.js"
   (should (equal (buffer-substring (point-min) (point-max))
                  context-coloring-test-expected-nested))))

(ert-deftest context-coloring-test-vow ()
  (context-coloring-test-with-fixture "./fixtures/vow.js"
    (should (equal (buffer-substring (point-min) (point-max))
                   context-coloring-test-expected-vow))))

(provide 'context-coloring-test)
