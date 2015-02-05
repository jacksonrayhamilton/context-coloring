;; -*- lexical-binding: t; -*-

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

(defconst directory (file-name-directory (or load-file-name buffer-file-name)))

(defun resolve-path (path)
  (expand-file-name path directory))

(let ((files '("https://raw.githubusercontent.com/mooz/js2-mode/master/js2-mode.el"
               "https://raw.githubusercontent.com/rejeep/ert-async.el/master/ert-async.el")))
  (make-directory (resolve-path "../libraries") t)
  (dolist (file files)
    (let* ((basename (file-name-nondirectory file))
           (destination (resolve-path (concat "../libraries/" basename))))
      (when (null (file-exists-p destination))
        (with-current-buffer (url-retrieve-synchronously file)
          (write-file destination))))))
