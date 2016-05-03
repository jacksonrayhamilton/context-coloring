;;; context-coloring-benchmark.el --- Benchmarks for context coloring  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2016  Free Software Foundation, Inc.

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
(require 'context-coloring-javascript)
(require 'context-coloring-emacs-lisp)
(require 'elp)


(defconst context-coloring-benchmark-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun context-coloring-benchmark-resolve-path (path)
  "Resolve PATH from this file's directory."
  (expand-file-name path context-coloring-benchmark-path))

(defun context-coloring-benchmark-log-results (result-file fixture statistics)
  "Log results to RESULT-FILE for FIXTURE with STATISTICS."
  (let ((results (prog1
                     (progn
                       (elp-results)
                       (buffer-substring-no-properties (point-min) (point-max)))
                   (kill-buffer))))
    (make-directory (context-coloring-benchmark-resolve-path "./benchmark") t)
    (append-to-file
     (with-temp-buffer
       (goto-char (point-min))
       (insert (format "For fixture \"%s\":\n" fixture))
       (insert "\n")
       (insert "General statistics:\n")
       (insert (format "File size: %s bytes\n" (plist-get statistics :file-size)))
       (insert (format "Lines: %s\n" (plist-get statistics :lines)))
       (insert (format "Words: %s\n" (plist-get statistics :words)))
       (insert (format "Colorization times: %s\n"
                       (context-coloring-join
                        (mapcar (lambda (number)
                                  (format "%.4f" number))
                                (plist-get statistics :colorization-times)) ", ")))
       (insert (format "Average colorization time: %.4f\n"
                       (plist-get statistics :average-colorization-time)))
       (insert "\n")
       (insert "Function statistics:\n")
       (insert "(Function Name / Call Count / Elapsed Time / Average Time):\n")
       (insert results)
       (insert "\n")
       (buffer-substring-no-properties (point-min) (point-max)))
     nil result-file)))

(defun context-coloring-benchmark (title fixtures)
  "Execute a benchmark titled TITLE against FIXTURES."
  (let ((result-file (context-coloring-benchmark-resolve-path
                      (format "./benchmark/results-%s-%s.log"
                              title (format-time-string "%s")))))
    (mapc
     (lambda (path)
       (let ((fixture (context-coloring-benchmark-resolve-path path))
             colorization-start-time
             (colorization-times '())
             advice)
         (setq
          advice
          (let ((count 0))
            (lambda (original-function)
              (funcall original-function)
              (setq count (+ count 1))
              ;; First 5 runs are for gathering real coloring times,
              ;; unaffected by elp instrumentation.
              (when (<= count 5)
                (push (- (float-time) colorization-start-time) colorization-times))
              (cond
               ((= count 10)
                (advice-remove #'context-coloring-colorize advice)
                (context-coloring-benchmark-log-results
                 result-file
                 fixture
                 (list
                  :file-size (nth 7 (file-attributes fixture))
                  :lines (count-lines (point-min) (point-max))
                  :words (count-words (point-min) (point-max))
                  :colorization-times colorization-times
                  :average-colorization-time (/ (apply #'+ colorization-times) 5)))
                (elp-restore-all)
                (kill-buffer))
               ;; The last 5 runs are for gathering function call and
               ;; duration statistics.
               ((= count 5)
                (elp-instrument-package "context-coloring-")
                (context-coloring-colorize))
               (t
                (setq colorization-start-time (float-time))
                (context-coloring-colorize))))))
         (advice-add #'context-coloring-colorize :around advice)
         (setq colorization-start-time (float-time))
         (find-file fixture)))
     fixtures)))

(defconst context-coloring-benchmark-javascript-fixtures
  '("./fixtures/benchmark/jquery-2.1.1.js"
    "./fixtures/benchmark/lodash-2.4.1.js"
    "./fixtures/benchmark/async-0.9.0.js"
    "./fixtures/benchmark/mkdirp-0.5.0.js")
  "Arbitrary JavaScript files for performance scrutiny.")

(defun context-coloring-benchmark-js2-mode-run ()
  "Benchmark `js2-mode'."
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook #'context-coloring-mode)
  (let ((js2-mode-show-parse-errors nil)
        (js2-mode-show-strict-warnings nil))
    (context-coloring-benchmark
     "js2-mode"
     context-coloring-benchmark-javascript-fixtures))
  (setq auto-mode-alist (delete '("\\.js\\'" . js2-mode)
                                auto-mode-alist))
  (remove-hook 'js2-mode-hook #'context-coloring-mode))

(defconst context-coloring-benchmark-emacs-lisp-fixtures
  '("./fixtures/benchmark/lisp.el"
    "./fixtures/benchmark/faces.el"
    "./fixtures/benchmark/subr.el"
    "./fixtures/benchmark/simple.el")
  "Arbitrary Emacs Lisp files for performance scrutiny.")

(defun context-coloring-benchmark-emacs-lisp-mode-run ()
  "Benchmark `emacs-lisp-mode', then call CALLBACK."
  (add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)
  (context-coloring-benchmark
   "emacs-lisp-mode"
   context-coloring-benchmark-emacs-lisp-fixtures)
  (remove-hook 'emacs-lisp-mode-hook #'context-coloring-mode))

(defun context-coloring-benchmark-run ()
  "Benchmark all modes, then exit."
  (context-coloring-benchmark-js2-mode-run)
  (context-coloring-benchmark-emacs-lisp-mode-run)
  (kill-emacs))

(provide 'context-coloring-benchmark)

;;; context-coloring-benchmark.el ends here
