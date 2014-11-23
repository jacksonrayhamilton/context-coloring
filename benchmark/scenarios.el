;; Setup
(add-hook 'js-mode-hook 'context-coloring-mode)
(elp-instrument-package "context-coloring-")

;; Test 5 times.
(find-file (expand-file-name
            "./fixtures/jquery-2.1.1.js"
            (file-name-directory (or load-file-name buffer-file-name))))
(dotimes (n 4)
  (sit-for 2)
  (revert-buffer t t))
(sit-for 2)
(elp-results)
