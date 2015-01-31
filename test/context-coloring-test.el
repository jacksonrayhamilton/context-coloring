;; -*- lexical-binding: t; -*-

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

(defun context-coloring-test-setup ()
  (setq context-coloring-comments-and-strings nil))

(defun context-coloring-test-cleanup ()
  (setq context-coloring-comments-and-strings t)
  (setq context-coloring-after-colorize-hook nil)
  (setq context-coloring-js-block-scopes nil))

(defmacro context-coloring-test-with-fixture (fixture &rest body)
  "Evaluate BODY in a temporary buffer with the relative
FIXTURE."
  `(with-temp-buffer
     (unwind-protect
         (progn
           (context-coloring-test-setup)
           (insert (context-coloring-test-read-file ,fixture))
           ,@body)
       (context-coloring-test-cleanup))))

(defun context-coloring-test-with-temp-buffer-async (callback)
  "Create a temporary buffer, and evaluate CALLBACK there. A
teardown callback is passed to CALLBACK for it to invoke when it
is done."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (let ((previous-buffer (current-buffer))
          (temp-buffer (generate-new-buffer " *temp*")))
      (set-buffer temp-buffer)
      (funcall
       callback
       (lambda ()
         (and (buffer-name temp-buffer)
              (kill-buffer temp-buffer))
         (set-buffer previous-buffer))))))

(defun context-coloring-test-with-fixture-async (fixture callback &optional setup)
  "Evaluate CALLBACK in a temporary buffer with the relative
FIXTURE. A teardown callback is passed to CALLBACK for it to
invoke when it is done. An optional SETUP callback can be passed
to run arbitrary code before the mode is invoked."
  (context-coloring-test-with-temp-buffer-async
   (lambda (done-with-temp-buffer)
     (context-coloring-test-setup)
     (if setup (funcall setup))
     (insert (context-coloring-test-read-file fixture))
     (funcall
      callback
      (lambda ()
        (context-coloring-test-cleanup)
        (funcall done-with-temp-buffer))))))

(defun context-coloring-test-js-mode (fixture callback &optional setup)
  (context-coloring-test-with-fixture-async
   fixture
   (lambda (done-with-test)
     (js-mode)
     (context-coloring-mode)
     (context-coloring-colorize
      (lambda ()
        (funcall callback done-with-test))))
   setup))

(defmacro context-coloring-test-js2-mode (fixture &rest body)
  `(context-coloring-test-with-fixture
    ,fixture
    (require 'js2-mode)
    (setq js2-mode-show-parse-errors nil)
    (setq js2-mode-show-strict-warnings nil)
    (js2-mode)
    (context-coloring-mode)
    ,@body))

(defmacro context-coloring-test-assert-region (&rest body)
  `(let ((i 0)
         (length (- end start)))
     (while (< i length)
       (let* ((point (+ i start))
              (face (get-text-property point 'face))
              actual-level)
         ,@body)
       (setq i (+ i 1)))))

(defconst context-coloring-test-level-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face")

(defun context-coloring-test-assert-region-level (start end level)
  (context-coloring-test-assert-region
   (when (not (when face
                (let* ((face-string (symbol-name face))
                       (matches (string-match context-coloring-test-level-regexp face-string)))
                  (when matches
                    (setq actual-level (string-to-number (substring face-string
                                                                    (match-beginning 1)
                                                                    (match-end 1))))
                    (= level actual-level)))))
     (ert-fail (format "Expected level in region [%s, %s), which is \"%s\", to be %s; but at point %s, it was %s"
                       start end (buffer-substring-no-properties start end) level point actual-level)))))

(defun context-coloring-test-assert-region-face (start end expected-face)
  (context-coloring-test-assert-region
   (when (not (eq face expected-face))
     (ert-fail (format "Expected face in region [%s, %s), which is \"%s\", to be %s; but at point %s, it was %s"
                       start end (buffer-substring-no-properties start end) expected-face point face)))))

(defun context-coloring-test-assert-region-comment-delimiter (start end)
  (context-coloring-test-assert-region-face start end 'font-lock-comment-delimiter-face))

(defun context-coloring-test-assert-region-comment (start end)
  (context-coloring-test-assert-region-face start end 'font-lock-comment-face))

(defun context-coloring-test-assert-region-string (start end)
  (context-coloring-test-assert-region-face start end 'font-lock-string-face))

(defun context-coloring-test-assert-message (expected)
  (with-current-buffer "*Messages*"
    (let ((messages (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n")))
      (let ((message (car (nthcdr (- (length messages) 2) messages))))
        (should (equal message expected))))))

(ert-deftest context-coloring-test-unsupported-mode ()
  (context-coloring-test-with-fixture
   "./fixtures/function-scopes.js"
   (context-coloring-mode)
   (context-coloring-test-assert-message
    "Context coloring is not available for this major mode")))

(defun context-coloring-test-js-function-scopes ()
  (context-coloring-test-assert-region-level 1 9 0)
  (context-coloring-test-assert-region-level 9 23 1)
  (context-coloring-test-assert-region-level 23 25 0)
  (context-coloring-test-assert-region-level 25 34 1)
  (context-coloring-test-assert-region-level 34 35 0)
  (context-coloring-test-assert-region-level 35 52 1)
  (context-coloring-test-assert-region-level 52 66 2)
  (context-coloring-test-assert-region-level 66 72 1)
  (context-coloring-test-assert-region-level 72 81 2)
  (context-coloring-test-assert-region-level 81 82 1)
  (context-coloring-test-assert-region-level 82 87 2)
  (context-coloring-test-assert-region-level 87 89 1))

(ert-deftest-async context-coloring-test-js-mode-function-scopes (done)
  (context-coloring-test-js-mode
   "./fixtures/function-scopes.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-function-scopes)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-function-scopes ()
  (context-coloring-test-js2-mode
   "./fixtures/function-scopes.js"
   (context-coloring-test-js-function-scopes)))

(defun context-coloring-test-js-global ()
  (context-coloring-test-assert-region-level 20 28 1)
  (context-coloring-test-assert-region-level 28 35 0)
  (context-coloring-test-assert-region-level 35 41 1))

(ert-deftest-async context-coloring-test-js-mode-global (done)
  (context-coloring-test-js-mode
   "./fixtures/global.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-global)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-global ()
  (context-coloring-test-js2-mode
   "./fixtures/global.js"
   (context-coloring-test-js-global)))

(defun context-coloring-test-js-block-scopes ()
  (context-coloring-test-assert-region-level 20 64 1)
   (setq context-coloring-js-block-scopes t)
   (context-coloring-colorize)
   (context-coloring-test-assert-region-level 20 27 1)
   (context-coloring-test-assert-region-level 27 41 2)
   (context-coloring-test-assert-region-level 41 42 1)
   (context-coloring-test-assert-region-level 42 64 2))

(ert-deftest context-coloring-test-js2-mode-block-scopes ()
  (context-coloring-test-js2-mode
   "./fixtures/block-scopes.js"
   (context-coloring-test-js-block-scopes)))

(defun context-coloring-test-js-catch ()
  (context-coloring-test-assert-region-level 20 27 1)
  (context-coloring-test-assert-region-level 27 51 2)
  (context-coloring-test-assert-region-level 51 52 1)
  (context-coloring-test-assert-region-level 52 73 2)
  (context-coloring-test-assert-region-level 73 101 3)
  (context-coloring-test-assert-region-level 101 102 1)
  (context-coloring-test-assert-region-level 102 117 3)
  (context-coloring-test-assert-region-level 117 123 2))

(ert-deftest-async context-coloring-test-js-mode-catch (done)
  (context-coloring-test-js-mode
   "./fixtures/catch.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-catch)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-catch ()
  (context-coloring-test-js2-mode
   "./fixtures/catch.js"
   (context-coloring-test-js-catch)))

(defun context-coloring-test-js-key-names ()
  (context-coloring-test-assert-region-level 20 63 1))

(ert-deftest-async context-coloring-test-js-mode-key-names (done)
  (context-coloring-test-js-mode
   "./fixtures/key-names.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-key-names)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-key-names ()
  (context-coloring-test-js2-mode
   "./fixtures/key-names.js"
   (context-coloring-test-js-key-names)))

(defun context-coloring-test-js-property-lookup ()
  (context-coloring-test-assert-region-level 20 26 0)
  (context-coloring-test-assert-region-level 26 38 1)
  (context-coloring-test-assert-region-level 38 44 0)
  (context-coloring-test-assert-region-level 44 52 1)
  (context-coloring-test-assert-region-level 57 63 0)
  (context-coloring-test-assert-region-level 63 74 1))

(ert-deftest-async context-coloring-test-js-mode-property-lookup (done)
  (context-coloring-test-js-mode
   "./fixtures/property-lookup.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-property-lookup)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-property-lookup ()
  (context-coloring-test-js2-mode
   "./fixtures/property-lookup.js"
   (context-coloring-test-js-property-lookup)))

(defun context-coloring-test-js-key-values ()
  (context-coloring-test-assert-region-level 78 79 1))

(ert-deftest-async context-coloring-test-js-mode-key-values (done)
  (context-coloring-test-js-mode
   "./fixtures/key-values.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-key-values)
       (funcall teardown))
     (funcall done))))

(ert-deftest context-coloring-test-js2-mode-key-values ()
  (context-coloring-test-js2-mode
   "./fixtures/key-values.js"
   (context-coloring-test-js-key-values)))

(defun context-coloring-test-js-comments-and-strings ()
  (context-coloring-test-assert-region-comment-delimiter 1 4)
  (context-coloring-test-assert-region-comment 4 8)
  (context-coloring-test-assert-region-comment-delimiter 9 12)
  (context-coloring-test-assert-region-comment 12 19)
  (context-coloring-test-assert-region-string 20 32)
  (context-coloring-test-assert-region-level 32 33 0))

(ert-deftest-async context-coloring-test-js-mode-comments-and-strings (done)
  (context-coloring-test-js-mode
   "./fixtures/comments-and-strings.js"
   (lambda (teardown)
     (unwind-protect
         (context-coloring-test-js-comments-and-strings)
       (funcall teardown))
     (funcall done))
   (lambda ()
     (setq context-coloring-comments-and-strings t))))

(ert-deftest context-coloring-test-js2-mode-comments-and-strings ()
  (context-coloring-test-js2-mode
   "./fixtures/comments-and-strings.js"
   (setq context-coloring-comments-and-strings t)
   (context-coloring-colorize)
   (context-coloring-test-js-comments-and-strings)))

(provide 'context-coloring-test)
