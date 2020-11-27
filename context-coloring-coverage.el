;;; context-coloring-coverage.el --- Test coverage for context coloring  -*- lexical-binding: t; -*-

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

;; Test coverage support for context coloring.

;; Use with `make cover'.

;;; Code:

(require 'json)
(require 'undercover nil 'noerror)   ;Don't signal an error during compilation.


(defconst context-coloring-coverage-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun context-coloring-coverage-resolve-path (path)
  "Resolve PATH from this file's directory."
  (expand-file-name path context-coloring-coverage-directory))

(defconst context-coloring-coverage-output-file-prefix
  (format-time-string "%s"))

(defconst context-coloring-coverage-output-directory
  (context-coloring-coverage-resolve-path "./coverage/"))

(defconst context-coloring-coverage-output-file
  (concat context-coloring-coverage-output-directory
          context-coloring-coverage-output-file-prefix ".json"))

(defconst context-coloring-coverage-report-file
  (concat context-coloring-coverage-output-directory
          context-coloring-coverage-output-file-prefix ".txt"))

(defun context-coloring-coverage-join (strings delimiter)
  "Join a list of STRINGS with the string DELIMITER."
  (mapconcat #'identity strings delimiter))

(defun context-coloring-coverage-percentage (dividend divisor)
  "Get the percentage of DIVIDEND / DIVISOR with precision 2."
  (let ((percentage (/ (float (round (* (/ (float dividend) divisor) 10000))) 100)))
    (number-to-string
     (cond
      ((= (mod percentage 1) 0)
       ;; Get an integer because we don't like dangling zeros.
       (round percentage))
      (t
       percentage)))))

(defun context-coloring-coverage-format-source-file (source-file)
  "Generate a report for SOURCE-FILE's line coverage."
  (let* ((source-lines (split-string (cdr (assq 'source source-file)) "\n"))
         (coverage (cdr (assq 'coverage source-file)))
         (results (list "Hits  | Source"
                        (context-coloring-coverage-join (make-vector 80 "-") "")))
         (lines-hit 0)
         (lines-hittable 0)
         hits
         source-line)
    (while coverage
      (setq hits (car coverage))
      (setq coverage (cdr coverage))
      (setq source-line (car source-lines))
      (setq source-lines (cdr source-lines))
      (when (not (null hits))
        (setq lines-hittable (+ lines-hittable 1))
        (when (> hits 0)
          (setq lines-hit (+ lines-hit 1))))
      (setq results
            (append results
                    (list (format
                           "%-5s %s %s"
                           (if hits hits "N/A")
                           (if (and hits (= hits 0)) "~" "|")
                           source-line)))))
    (setq results
          (append results
                  (list
                   ""
                   (format
                    "Lines: %s / %s"
                    lines-hit
                    lines-hittable)
                   (format
                    "Coverage: %s%%"
                    (context-coloring-coverage-percentage lines-hit lines-hittable)))))
    (context-coloring-coverage-join results "\n")))

(defun context-coloring-coverage-format (coverage-data)
  "Generate reports for all files in COVERAGE-DATA."
  (context-coloring-coverage-join
   (mapcar
    #'context-coloring-coverage-format-source-file
    (cdr (assq 'source_files coverage-data)))
   "\n\n"))

(defun context-coloring-coverage-local-init ()
  "Initialize test coverage for local viewing."
  (make-directory context-coloring-coverage-output-directory t)
  (setq undercover-force-coverage t)
  (setenv "COVERALLS_REPO_TOKEN" "noop")
  (undercover "*.el"
              (:report-file context-coloring-coverage-output-file)
              (:send-report nil))
  (add-hook
   'kill-emacs-hook
   (lambda ()
     (let (original-json-array-type
           coverage-data
           report)
       (with-temp-buffer
         (insert-file-contents-literally context-coloring-coverage-output-file)
         (setq original-json-array-type json-array-type)
         (setq json-array-type 'list)
         (setq coverage-data
               (json-read-from-string
                (buffer-substring-no-properties (point-min) (point-max))))
         (setq json-array-type original-json-array-type)
         (setq report
               (context-coloring-coverage-format coverage-data))
         (setq report (concat report "\n")))
       (princ report)
       (with-temp-buffer
         (insert report)
         (write-file context-coloring-coverage-report-file))))
   t)
  (require 'context-coloring))

(defun context-coloring-coverage-ci-init ()
  "Initialize test coverage for continuous integration."
  (undercover "*.el")
  (require 'context-coloring))

(provide 'context-coloring-coverage)

;;; context-coloring-coverage.el ends here
