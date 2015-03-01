;;; scripts/download-dependencies.el --- Get files for development. -*- lexical-binding: t; -*-

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

;; Download dependencies for development.

;; Dependencies don't need to be version-controlled. They are also
;; bleeding-edge, which is good because that is what most MELPA users are using.

;;; Code:

(defconst download-dependencies-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defun download-dependencies-resolve-path (path)
  "Resolve a path relative to this file's directory."
  (expand-file-name path download-dependencies-directory))

(defun download-dependencies-strip-headers ()
  "Remove the http headers included in the output of
`url-retrieve-synchronously'."
  (goto-char 1)
  (kill-paragraph 1) ; The headers are 1 paragraph.  I hope.
  (kill-line))       ; A line separates the headers from the file's content.

(defun download-dependencies-get-dependencies ()
  "Read the `dependencies' file as a list of URLs."
  (with-temp-buffer
    (insert-file-contents (download-dependencies-resolve-path "./dependencies"))
    (split-string (buffer-substring-no-properties (point-min) (point-max)))))

(defun download-dependencies ()
  "Download dependencies for development."
  (let ((files (download-dependencies-get-dependencies)))
    (make-directory (download-dependencies-resolve-path "../libraries") t)
    (dolist (file files)
      (let* ((basename (file-name-nondirectory file))
             (destination (download-dependencies-resolve-path
                           (concat "../libraries/" basename))))
        (unless (file-exists-p destination)
          (with-current-buffer (url-retrieve-synchronously file)
            (download-dependencies-strip-headers)
            (write-file destination)))))))

;;; download-dependencies.el ends here
