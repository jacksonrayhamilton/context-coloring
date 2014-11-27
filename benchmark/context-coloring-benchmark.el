(defconst context-coloring-benchmark-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-benchmark-resolve-path (path)
  (expand-file-name path context-coloring-benchmark-path))

(defun context-coloring-benchmark-setup ()
  (setq context-coloring-benchmark-colorization t)
  (add-hook 'js-mode-hook 'context-coloring-mode)
  (elp-instrument-package "context-coloring-"))

(defun context-coloring-benchmark-teardown ()
  (setq context-coloring-benchmark-colorization nil)
  (remove-hook 'js-mode-hook 'context-coloring-mode))

(defun context-coloring-benchmark-run ()
  (context-coloring-benchmark-setup)

  (let ((result-file (context-coloring-benchmark-resolve-path
                      (concat "./results-" (format-time-string "%s") ".log"))))

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

        (elp-results)
        (let ((results-buffer (current-buffer)))
          (with-temp-buffer
            (insert (concat fixture "\n"))
            (prepend-to-buffer results-buffer (point-min) (point-max)))
          (with-temp-buffer
            (insert "\n")
            (append-to-buffer results-buffer (point-min) (point-max))))

        (append-to-file nil nil result-file))))

  (context-coloring-benchmark-teardown)
  (kill-emacs))
