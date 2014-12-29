;; -*- lexical-binding: t; -*-

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

(defun context-coloring-benchmark-next-tick (function)
  (run-at-time 0.001 nil function))

(defun context-coloring-benchmark-next (list continue stop)
  (if (null list)
      (context-coloring-benchmark-next-tick stop)
    (context-coloring-benchmark-next-tick
     (lambda ()
       (funcall
        continue
        (car list)
        (lambda ()
          (context-coloring-benchmark-next (cdr list) continue stop)))))))

(defun context-coloring-benchmark-async (title setup teardown fixtures callback)
  (funcall setup)
  (let ((result-file (context-coloring-benchmark-resolve-path
                      (concat "./results-" title "-" (format-time-string "%s") ".log"))))
    (context-coloring-benchmark-next
     fixtures
     (lambda (path next)
       (let ((fixture (context-coloring-benchmark-resolve-path path))
             advice)
         (setq
          advice
          (let ((count 0))
            (lambda (original-function)
              (funcall
               original-function
               (lambda ()
                 (setq count (+ count 1))
                 ;; Test 5 times.
                 (if (= count 5)
                     (progn
                       (advice-remove 'context-coloring-colorize advice)
                       (kill-buffer)
                       (context-coloring-benchmark-log-results
                        result-file
                        fixture)
                       (funcall next))
                   (funcall 'context-coloring-colorize)))))))
         (advice-add 'context-coloring-colorize :around advice)
         (find-file fixture)))
     (lambda ()
       (funcall teardown)
       (if callback (funcall callback))))))

(defconst context-coloring-benchmark-js-fixtures
  '("./fixtures/jquery-2.1.1.js"
    "./fixtures/lodash-2.4.1.js"
    "./fixtures/async-0.9.0.js"
    "./fixtures/mkdirp-0.5.0.js"))

(defun context-coloring-benchmark-js-mode-setup ()
  (add-hook 'js-mode-hook 'context-coloring-mode)
  (elp-instrument-package "context-coloring-"))

(defun context-coloring-benchmark-js-mode-teardown ()
  (remove-hook 'js-mode-hook 'context-coloring-mode))

(defun context-coloring-benchmark-js-mode-run (callback)
  (context-coloring-benchmark-async
   "js-mode"
   'context-coloring-benchmark-js-mode-setup
   'context-coloring-benchmark-js-mode-teardown
   context-coloring-benchmark-js-fixtures
   callback))

(defun context-coloring-benchmark-js2-mode-setup ()
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
  (setq js2-mode-show-parse-errors t))

(defun context-coloring-benchmark-js2-mode-run (callback)
  (context-coloring-benchmark-async
   "js2-mode"
   'context-coloring-benchmark-js2-mode-setup
   'context-coloring-benchmark-js2-mode-teardown
   context-coloring-benchmark-js-fixtures
   callback))

(defun context-coloring-benchmark-run ()
  (context-coloring-benchmark-next
   '(context-coloring-benchmark-js-mode-run
     context-coloring-benchmark-js2-mode-run)
   (lambda (function next)
     (funcall function next))
   (lambda ()
     (kill-emacs))))
