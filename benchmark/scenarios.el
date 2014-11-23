;; Setup
(add-hook 'js-mode-hook 'context-coloring-mode)
(elp-instrument-package "context-coloring-")

(dolist (path '("./fixtures/jquery-2.1.1.js"
                "./fixtures/lodash-2.4.1.js"
                "./fixtures/async-0.9.0.js"
                "./fixtures/mkdirp-0.5.0.js"))

  ;; Test 5 times.
  (find-file (expand-file-name
              path
              (file-name-directory (or load-file-name buffer-file-name))))
  (dotimes (n 4)
    (sit-for 2)
    (revert-buffer t t))
  (sit-for 2))

(elp-results)
