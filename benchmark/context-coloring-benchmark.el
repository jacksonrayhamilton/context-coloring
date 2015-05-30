;;; context-coloring-benchmark.el --- Benchmarks for context coloring  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Benchmarks for context coloring.

;; Use with `make bench'.

;;; Code:

(require 'context-coloring)
(require 'js2-mode)


(defconst context-coloring-benchmark-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun context-coloring-benchmark-resolve-path (path)
  "Resolve PATH from this file's directory."
  (expand-file-name path context-coloring-benchmark-path))

(defun context-coloring-benchmark-series (sequence callback)
  "Call each function in SEQUENCE, then call CALLBACK.  Each
function is passed a single callback parameter for it to call
when it is done."
  (cond
   ((null sequence)
    (funcall callback))
   (t
    (funcall
     (car sequence)
     (lambda ()
       (run-with-timer
        nil nil
        (lambda ()
          (context-coloring-benchmark-series
           (cdr sequence)
           callback))))))))

(defun context-coloring-benchmark-mapc (sequence iteratee callback)
  "For each element in SEQUENCE, call ITERATEE, finally call
CALLBACK.  ITERATEE is passed the current element and a callback
for it to call when it is done."
  (cond
   ((null sequence)
    (funcall callback))
   (t
    (funcall
     iteratee
     (car sequence)
     (lambda ()
       (run-with-timer
        nil nil
        (lambda ()
          (context-coloring-benchmark-mapc
           (cdr sequence)
           iteratee
           callback))))))))

(defun context-coloring-benchmark-log-results (result-file fixture)
  "Log benchmarking results to RESULT-FILE for fixture FIXTURE."
  (elp-results)
  (let ((results-buffer (current-buffer)))
    (with-temp-buffer
      (insert (concat fixture "\n"))
      (prepend-to-buffer results-buffer (point-min) (point-max)))
    (with-temp-buffer
      (insert "\n")
      (append-to-buffer results-buffer (point-min) (point-max))))
  (make-directory (context-coloring-benchmark-resolve-path "./logs") t)
  (append-to-file nil nil result-file))

(defun context-coloring-benchmark (title setup teardown fixtures callback)
  "Execute a benchmark titled TITLE with SETUP and TEARDOWN
callbacks.  Measure the performance of all FIXTURES, calling
CALLBACK when all are done."
  (funcall setup)
  (elp-instrument-package "context-coloring-")
  (let ((result-file (context-coloring-benchmark-resolve-path
                      (format "./logs/results-%s-%s.log"
                              title (format-time-string "%s")))))
    (context-coloring-benchmark-mapc
     fixtures
     (lambda (path callback)
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
                 (cond
                  ((= count 5)
                   (advice-remove 'context-coloring-colorize advice)
                   (kill-buffer)
                   (context-coloring-benchmark-log-results
                    result-file
                    fixture)
                   (funcall callback))
                  (t
                   (funcall 'context-coloring-colorize))))))))
         (advice-add 'context-coloring-colorize :around advice)
         (find-file fixture)))
     (lambda ()
       (funcall teardown)
       (funcall callback)))))

(defconst context-coloring-benchmark-js-fixtures
  '("./fixtures/jquery-2.1.1.js"
    "./fixtures/lodash-2.4.1.js"
    "./fixtures/async-0.9.0.js"
    "./fixtures/mkdirp-0.5.0.js")
  "Arbitrary JavaScript files for performance scrutiny.")

(defun context-coloring-benchmark-js-mode-run (callback)
  "Benchmark `js-mode', then call CALLBACK."
  (context-coloring-benchmark
   "js-mode"
   (lambda ()
     "Preparation logic for `js-mode'."
     (add-hook 'js-mode-hook 'context-coloring-mode))
   (lambda ()
     "Cleanup logic for `js-mode'."
     (remove-hook 'js-mode-hook 'context-coloring-mode))
   context-coloring-benchmark-js-fixtures
   callback))

(defun context-coloring-benchmark-js2-mode-run (callback)
  "Benchmark `js2-mode', then call CALLBACK."
  (context-coloring-benchmark
   "js2-mode"
   (lambda ()
     "Preparation logic for `js2-mode'."
     (setq js2-mode-show-parse-errors nil)
     (setq js2-mode-show-strict-warnings nil)
     (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
     (add-hook 'js2-mode-hook 'context-coloring-mode))
   (lambda ()
     "Cleanup logic for `js2-mode'."
     (remove-hook 'js2-mode-hook 'context-coloring-mode)
     (setq auto-mode-alist (delete '("\\.js\\'" . js2-mode)
                                   auto-mode-alist))
     (setq js2-mode-show-strict-warnings t)
     (setq js2-mode-show-parse-errors t))
   context-coloring-benchmark-js-fixtures
   callback))

(defun context-coloring-benchmark-run ()
  "Benchmark all modes, then exit."
  (context-coloring-benchmark-series
   '(context-coloring-benchmark-js-mode-run
     context-coloring-benchmark-js2-mode-run)
   (lambda ()
     (kill-emacs))))

(provide 'context-coloring-benchmark)

;;; context-coloring-benchmark.el ends here
