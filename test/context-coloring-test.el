(defconst context-coloring-test-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-test-resolve-path (path)
  (expand-file-name path context-coloring-test-path))

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

(ert-deftest context-coloring-test-nested ()
  (context-coloring-test-with-fixture "./fixtures/nested.js"
   ))

(provide 'context-coloring-test)
