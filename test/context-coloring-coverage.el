;;; context-coloring-coverage.el --- Test coverage for context coloring  -*- lexical-binding: t; -*-

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

;; Test coverage support for context coloring.

;; Use with `make cover'.

;;; Code:

(require 'undercover)


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

(defconst context-coloring-coverage-parser
  (concat "node " (context-coloring-coverage-resolve-path "./parse-coverage.js")))

(defun context-coloring-coverage-local-init ()
  "Initialize test coverage for local viewing."
  (make-directory context-coloring-coverage-output-directory t)
  (setq undercover-force-coverage t)
  (setenv "COVERALLS_REPO_TOKEN" "noop")
  (undercover "context-coloring.el"
              (:report-file context-coloring-coverage-output-file))
  (add-hook
   'kill-emacs-hook
   (lambda ()
     (let* ((output-buffer (get-buffer-create "*parsed coverage*")))
       (call-process-shell-command
        context-coloring-coverage-parser
        context-coloring-coverage-output-file
        output-buffer)
       (with-current-buffer output-buffer
         (princ (buffer-substring-no-properties (point-min) (point-max)))
         (write-file context-coloring-coverage-report-file))))
   t)
  (require 'context-coloring))

(defun context-coloring-coverage-ci-init ()
  "Initialize test coverage for continuous integration."
  (undercover "context-coloring.el")
  (require 'context-coloring))

(provide 'context-coloring-coverage)

;; context-coloring-coverage.el ends here
