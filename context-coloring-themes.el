;;; context-coloring-themes.el --- Color schemes for Context Coloring. -*- lexical-binding: t; -*-

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

;; An assortment of color schemes for Context Coloring, many of which are based
;; on existing color themes and custom themes.

;; To use, simply call `context-coloring-load-theme':

;; (require 'context-coloring)
;; (context-coloring-load-theme 'zenburn)

;;; Code:

(defvar context-coloring-theme-hash-table (make-hash-table :test 'eq)
  "Mapping of theme names to theme properties.")

(defun context-coloring-define-theme (theme &rest properties)
  "Define a theme named THEME for coloring scope levels.
PROPERTIES is a property list specifiying the following details:

`:colors': List of colors that this theme uses."
  (puthash
   theme
   (lambda ()
     (apply 'context-coloring-set-colors (plist-get properties :colors)))
   context-coloring-theme-hash-table))

(defun context-coloring-load-theme (theme)
  "Apply THEME's colors and other properties for context
coloring."
  (let ((function (gethash theme context-coloring-theme-hash-table)))
    (when (null function)
      (error (format "No such theme `%s'" theme)))
    (funcall function)))

(context-coloring-define-theme
 'monokai
 :colors '("#F8F8F2"
           "#66D9EF"
           "#A1EFE4"
           "#A6E22E"
           "#E6DB74"
           "#FD971F"
           "#F92672"
           "#FD5FF0"
           "#AE81FF"))

(context-coloring-define-theme
 'solarized
 :colors '("#839496"
           "#268bd2"
           "#2aa198"
           "#859900"
           "#b58900"
           "#cb4b16"
           "#dc322f"
           "#d33682"
           "#6c71c4"
           "#69B7F0"
           "#69CABF"
           "#B4C342"
           "#DEB542"
           "#F2804F"
           "#FF6E64"
           "#F771AC"
           "#9EA0E5"))

(context-coloring-define-theme
 'tango
 :colors '("#2e3436"
           "#346604"
           "#204a87"
           "#5c3566"
           "#a40000"
           "#b35000"
           "#c4a000"
           "#8ae234"
           "#8cc4ff"
           "#ad7fa8"
           "#ef2929"
           "#fcaf3e"
           "#fce94f"))

(context-coloring-define-theme
 'zenburn
 :colors '("#DCDCCC"
           "#93E0E3"
           "#BFEBBF"
           "#F0DFAF"
           "#DFAF8F"
           "#CC9393"
           "#DC8CC3"
           "#94BFF3"
           "#9FC59F"
           "#D0BF8F"
           "#DCA3A3"))

(provide 'context-coloring-themes)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; context-coloring-themes.el ends here
