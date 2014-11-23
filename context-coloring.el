;;; context-coloring.el --- JavaScript syntax highlighting, except not for syntax.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Keywords: context coloring highlighting js javascript
;; Version: 1.0.0
;; Package-Requires: ((emacs "24"))

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

;; Highlights JavaScript code according to function context.
;;
;; Usage:
;;
;; Install Node.js 0.10+.
;; Run `make` in this file's directory.
;; In your ~/.emacs:
;;
;; (require 'context-coloring)
;; (add-hook 'js-mode-hook 'context-coloring-mode)

;;; Code:

;;; Faces

(defface context-coloring-depth--1-face
  '((((background light)) (:foreground "#7f7f7f"))
    (((background dark)) (:foreground "#7f7f7f")))
  "Context coloring face, depth -1; comments."
  :group 'context-coloring-faces)

(defface context-coloring-depth-0-face
  '((((background light)) (:foreground "#000000"))
    (((background dark)) (:foreground "#ffffff")))
  "Context coloring face, depth 0; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-1-face
  '((((background light)) (:foreground "#2D6994"))
    (((background dark)) (:foreground "#ffff80")))
  "Context coloring face, depth 1."
  :group 'context-coloring-faces)

(defface context-coloring-depth-2-face
  '((((background light)) (:foreground "#592D94"))
    (((background dark)) (:foreground "#cdfacd")))
  "Context coloring face, depth 2."
  :group 'context-coloring-faces)

(defface context-coloring-depth-3-face
  '((((background light)) (:foreground "#A13143"))
    (((background dark)) (:foreground "#d8d8ff")))
  "Context coloring face, depth 3."
  :group 'context-coloring-faces)

(defface context-coloring-depth-4-face
  '((((background light)) (:foreground "#AC7135"))
    (((background dark)) (:foreground "#e7c7ff")))
  "Context coloring face, depth 4."
  :group 'context-coloring-faces)

(defface context-coloring-depth-5-face
  '((((background light)) (:foreground "#ACA135"))
    (((background dark)) (:foreground "#ffcdcd")))
  "Context coloring face, depth 5."
  :group 'context-coloring-faces)

(defface context-coloring-depth-6-face
  '((((background light)) (:foreground "#539A2F"))
    (((background dark)) (:foreground "#ffe390")))
  "Context coloring face, depth 6."
  :group 'context-coloring-faces)

(defconst context-coloring-face-count 7
  "Number of faces defined for highlighting delimiter levels.
Determines depth at which to cycle through faces again.")

(defface context-coloring-depth--1-italic-face
  '((default (:inherit context-coloring-depth--1-face :slant italic)))
  "Context coloring face, depth -1; italic; comments."
  :group 'context-coloring-faces)

(defface context-coloring-depth-0-bold-face
  '((default (:inherit context-coloring-depth-0-face :weight bold)))
  "Context coloring face, depth 0; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-1-bold-face
  '((default (:inherit context-coloring-depth-1-face :weight bold)))
  "Context coloring face, depth 1; bold."
  :group 'context-coloring-faces)

(defface context-coloring-depth-2-bold-face
  '((default (:inherit context-coloring-depth-2-face :weight bold)))
  "Context coloring face, depth 2; bold."
  :group 'context-coloring-faces)

(defface context-coloring-depth-3-bold-face
  '((default (:inherit context-coloring-depth-3-face :weight bold)))
  "Context coloring face, depth 3; bold."
  :group 'context-coloring-faces)

(defface context-coloring-depth-4-bold-face
  '((default (:inherit context-coloring-depth-4-face :weight bold)))
  "Context coloring face, depth 4; bold."
  :group 'context-coloring-faces)

(defface context-coloring-depth-5-bold-face
  '((default (:inherit context-coloring-depth-5-face :weight bold)))
  "Context coloring face, depth 5; bold."
  :group 'context-coloring-faces)

(defface context-coloring-depth-6-bold-face
  '((default (:inherit context-coloring-depth-6-face :weight bold)))
  "Context coloring face, depth 6; bold."
  :group 'context-coloring-faces)


;;; Face functions

(defsubst context-coloring-level-face (depth style)
  "Return face-name for DEPTH and STYLE as a string \"context-coloring-depth-DEPTH-face\".
For example: \"context-coloring-depth-1-face\"."
  (intern-soft
   (concat "context-coloring-depth-"
           (number-to-string
            (or
             ;; Has a face directly mapping to it.
             (and (< depth context-coloring-face-count)
                  depth)
             ;; After the number of available faces are used up, pretend the 0th
             ;; face doesn't exist.
             (+ 1
                (mod (- depth 1)
                     (- context-coloring-face-count 1)))))
           (cond ((= 1 style) "-bold")
                 ((= 2 style) "-italic")
                 (t ""))
           "-face")))


;;; Customizable variables

(defcustom context-coloring-delay 0.25
  "Delay between a buffer update and colorization.

If your performance is poor, you might want to increase this."
  :group 'context-coloring)


;;; Local variables

(defvar context-coloring-buffer nil
  "Reference to this buffer (for timers).")
(make-variable-buffer-local 'context-coloring-buffer)

(defvar context-coloring-scopifier-process nil
  "Only allow a single scopifier process to run at a time. This
is a reference to that one process.")
(make-variable-buffer-local 'context-coloring-scopifier-process)

(defvar context-coloring-colorize-idle-timer nil
  "Reference to currently-running idle timer.")
(make-variable-buffer-local 'context-coloring-colorize-idle-timer)

(defvar context-coloring-changed nil
  "Indication that the buffer has changed recently, which would
imply that it should be colorized again.")
(make-variable-buffer-local 'context-coloring-changed)


;;; Scopification

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defconst context-coloring-scopifier-path
  (expand-file-name "./bin/scopifier" context-coloring-path)
  "Path to the external scopifier executable.")

(defun context-coloring-apply-tokens (tokens)
  "Processes TOKENS to apply context-based coloring to the
current buffer. Tokens are vectors consisting of 4 integers:
start, end, level, and style."
  (with-silent-modifications
    ;; Reset in case there should be uncolored areas.
    (remove-text-properties (point-min) (point-max) `(face nil rear-nonsticky nil))
    (let ((i 0)
          (len (length tokens)))
      (while (< i len)
        (add-text-properties
         (elt tokens i)
         (elt tokens (+ i 1))
         `(face ,(context-coloring-level-face
                  (elt tokens (+ i 2))
                  (elt tokens (+ i 3))) rear-nonsticky t))
        (setq i (+ i 4))))))

(defsubst context-coloring-kill-scopifier ()
  "Kills the currently-running scopifier process for this
buffer."
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-parse-array (input)
  "Specialized alternative JSON parser."
  (vconcat (mapcar 'string-to-number (split-string (substring input 1 -1) ","))))

(defun context-coloring-scopify ()
  "Invokes the external scopifier with the current buffer's
contents, reading the scopifier's response asynchronously and
applying a parsed list of tokens to
`context-coloring-apply-tokens'."

  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-kill-scopifier)

  ;; Start the process.
  (setq context-coloring-scopifier-process
        (start-process-shell-command "scopifier" nil context-coloring-scopifier-path))

  (let ((output "")
        (buffer context-coloring-buffer))

    ;; The process may produce output in multiple chunks. This filter
    ;; accumulates the chunks into a message.
    (set-process-filter context-coloring-scopifier-process
                        (lambda (process chunk)
                          (setq output (concat output chunk))))

    ;; When the process's message is complete, this sentinel parses it as JSON
    ;; and applies the tokens to the buffer.
    (set-process-sentinel context-coloring-scopifier-process
                          (lambda (process event)
                            (when (equal "finished\n" event)
                              (let ((tokens (context-coloring-parse-array output)))
                                (with-current-buffer buffer
                                  (context-coloring-apply-tokens tokens))
                                (setq context-coloring-scopifier-process nil))))))

  ;; Give the process its input.
  (process-send-region context-coloring-scopifier-process (point-min) (point-max))
  (process-send-eof context-coloring-scopifier-process))


;;; Colorization

(defun context-coloring-colorize ()
  "Colors the current buffer by function context."
  (interactive)
  (context-coloring-scopify))

(defun context-coloring-change-function (start end length)
  "Registers a change so that a context-colored buffer can be
colorized soon."
  ;; Tokenization is obsolete if there was a change.
  (context-coloring-kill-scopifier)
  (setq context-coloring-changed t))

(defun context-coloring-maybe-colorize ()
  "Colorize unders certain conditions. This will run as an idle
timer, so firstly the buffer must not be some other
buffer. Additionally, the buffer must have changed, otherwise
colorizing would be redundant."
  (when (and (eq context-coloring-buffer (window-buffer (selected-window)))
             context-coloring-changed)
    (setq context-coloring-changed nil)
    (context-coloring-colorize)))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring for JavaScript, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        (context-coloring-kill-scopifier)
        (when (not (null 'context-coloring-colorize-idle-timer))
          (cancel-timer context-coloring-colorize-idle-timer))
        (remove-hook 'after-change-functions 'context-coloring-change-function t)
        (font-lock-mode)
        (jit-lock-mode t))

    (setq context-coloring-buffer (current-buffer))

    ;; Colorize once initially.
    (context-coloring-colorize)

    ;; Font lock is not compatible with this mode; the converse is also true.
    (font-lock-mode 0)
    (jit-lock-mode nil)

    ;; Only recolor on change.
    (add-hook 'after-change-functions 'context-coloring-change-function nil t)

    ;; Only recolor idly.
    (setq context-coloring-colorize-idle-timer
          (run-with-idle-timer context-coloring-delay t 'context-coloring-maybe-colorize))))

;;;###autoload
(defun context-coloring-mode-enable ()
  (context-coloring-mode 1))

;;;###autoload
(defun context-coloring-mode-disable ()
  (context-coloring-mode 0))

;;;###autoload
(define-globalized-minor-mode global-context-coloring-mode
  context-coloring-mode context-coloring-mode-enable)

(provide 'context-coloring)

;;; context-coloring.el ends here
