(defconst context-coloring-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun context-coloring-test-resolve-path (path)
  "Resolve PATH from this file's directory."
  (expand-file-name path context-coloring-test-directory))

(defconst context-coloring-coverage-output-file-prefix
  (format-time-string "%s"))

(defconst context-coloring-coverage-output-file
  (context-coloring-test-resolve-path
   (concat "./coverage/" context-coloring-coverage-output-file-prefix ".json")))

(defconst context-coloring-coverage-report-file
  (context-coloring-test-resolve-path
   (concat "./coverage/" context-coloring-coverage-output-file-prefix ".txt")))

(defconst context-coloring-test-coverage-parser
  (concat "node " (context-coloring-test-resolve-path "./parse-coverage.js")))

(require 'undercover)
(setq undercover-force-coverage t)
(make-directory (context-coloring-test-resolve-path "./coverage/") t)
(undercover "context-coloring.el"
            (:report-file context-coloring-coverage-output-file))

(add-hook
 'kill-emacs-hook
 (lambda ()
   (let* ((output-buffer (get-buffer-create "*parsed coverage*")))
     (call-process-shell-command
      context-coloring-test-coverage-parser
      context-coloring-coverage-output-file
      output-buffer)
     (with-current-buffer output-buffer
       (princ (buffer-substring-no-properties (point-min) (point-max)))
       (write-file context-coloring-coverage-report-file))))
 t)

(require 'context-coloring)
