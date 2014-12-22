(defconst context-coloring-benchmark-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-benchmark-resolve-path (path)
  (expand-file-name path context-coloring-benchmark-path))

(defun context-coloring-benchmark-log-results (result-file fixture)
  (elp-results)
  (let ((results-buffer (current-buffer)))
    (with-temp-buffer
      (insert (concat fixture "\n"))
      (prepend-to-buffer results-buffer (point-min) (point-max)))
    (with-temp-buffer
      (insert "\n")
      (append-to-buffer results-buffer (point-min) (point-max))))
  (append-to-file nil nil result-file))

(defun context-coloring-benchmark-js-mode-setup ()
  (add-hook 'js-mode-hook 'context-coloring-mode)
  (elp-instrument-package "context-coloring-"))

(defun context-coloring-benchmark-js-mode-teardown ()
  (remove-hook 'js-mode-hook 'context-coloring-mode))

(defun context-coloring-benchmark-js-mode-run ()
  (context-coloring-benchmark-js-mode-setup)
  (let ((result-file (context-coloring-benchmark-resolve-path
                      (concat "./results-js-mode-" (format-time-string "%s") ".log"))))
    (dolist (path '("./fixtures/jquery-2.1.1.js"
                    "./fixtures/lodash-2.4.1.js"
                    "./fixtures/async-0.9.0.js"
                    "./fixtures/mkdirp-0.5.0.js"))
      (let ((fixture (context-coloring-benchmark-resolve-path path)))
        ;; Test 5 times.
        (find-file fixture)
        (dotimes (n 4)
          (sit-for 1)
          (revert-buffer t t))
        (sit-for 1)
        (context-coloring-benchmark-log-results result-file fixture))))
  (context-coloring-benchmark-js-mode-teardown))

(defun context-coloring-benchmark-js2-mode-setup ()
  (add-to-list 'load-path (context-coloring-benchmark-resolve-path
                           "../lib/js2-mode"))
  (require 'js2-mode)
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'context-coloring-mode)
  (elp-instrument-package "context-coloring-"))

(defun context-coloring-benchmark-js2-mode-teardown ()
  (remove-hook 'js2-mode-hook 'context-coloring-mode)
  (setq auto-mode-alist (delete '("\\.js\\'" . js2-mode)
                                auto-mode-alist))
  (setq js2-mode-show-strict-warnings t)
  (setq js2-mode-show-parse-errors t)
  (setq load-path (delete (context-coloring-benchmark-resolve-path
                           "../test/fixtures/js2-mode")
                          load-path)))

(defun context-coloring-benchmark-js2-mode-run ()
  (context-coloring-benchmark-js2-mode-setup)
  (let ((result-file (context-coloring-benchmark-resolve-path
                      (concat "./results-js2-mode-" (format-time-string "%s") ".log"))))
    (dolist (path '("./fixtures/jquery-2.1.1.js"
                    "./fixtures/lodash-2.4.1.js"
                    "./fixtures/async-0.9.0.js"
                    "./fixtures/mkdirp-0.5.0.js"))
      (let ((fixture (context-coloring-benchmark-resolve-path path)))
        ;; Test 5 times.
        (find-file fixture)
        (dotimes (n 4)
          (revert-buffer t t))
        (context-coloring-benchmark-log-results result-file fixture))))
  (context-coloring-benchmark-js2-mode-teardown))

(defun context-coloring-benchmark-run ()
  ;; (context-coloring-benchmark-js-mode-run)
  (context-coloring-benchmark-js2-mode-run)
  (kill-emacs))
