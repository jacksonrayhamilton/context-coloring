;;; context-coloring.el --- JavaScript syntax highlighting for grown-ups. -*- lexical-binding: t; -*-

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

;; Highlights JavaScript code according to function context. Code in the global
;; scope is white, code in functions within the global scope is yellow, code
;; within such functions is green, etc.
;;
;; Usage:
;;
;; Install Node.js 0.10.
;;
;; Put context-coloring.el on your load path. Then, in your ~/.emacs:
;;
;; (require 'context-coloring)
;; (add-hook 'js-mode-hook 'context-coloring-mode)

;;; Code:

(require 'json)

;;; Faces

(defface context-coloring-depth--1-face
  '((((background light)) (:foreground "#7f7f7f"))
    (((background dark)) (:foreground "#7f7f7f")))
  "Nested blocks face, depth -1; comments."
  :group 'context-coloring-faces)

(defface context-coloring-depth-0-face
  '((((background light)) (:foreground "#000000"))
    (((background dark)) (:foreground "#ffffff")))
  "Nested blocks face, depth 0; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-1-face
  '((((background light)) (:foreground "#2D6994"))
    (((background dark)) (:foreground "#ffff80")))
  "Nested blocks face, depth 1."
  :group 'context-coloring-faces)

(defface context-coloring-depth-2-face
  '((((background light)) (:foreground "#592D94"))
    (((background dark)) (:foreground "#cdfacd")))
  "Nested blocks face, depth 2."
  :group 'context-coloring-faces)

(defface context-coloring-depth-3-face
  '((((background light)) (:foreground "#A13143"))
    (((background dark)) (:foreground "#d8d8ff")))
  "Nested blocks face, depth 3."
  :group 'context-coloring-faces)

(defface context-coloring-depth-4-face
  '((((background light)) (:foreground "#AC7135"))
    (((background dark)) (:foreground "#e7c7ff")))
  "Nested blocks face, depth 4."
  :group 'context-coloring-faces)

(defface context-coloring-depth-5-face
  '((((background light)) (:foreground "#ACA135"))
    (((background dark)) (:foreground "#ffcdcd")))
  "Nested blocks face, depth 5."
  :group 'context-coloring-faces)

(defface context-coloring-depth-6-face
  '((((background light)) (:foreground "#539A2F"))
    (((background dark)) (:foreground "#ffe390")))
  "Nested blocks face, depth 6."
  :group 'context-coloring-faces)

(defconst context-coloring-face-count 7
  "Number of faces defined for highlighting delimiter levels.
Determines depth at which to cycle through faces again.")

(defface context-coloring-depth--1-italic-face
  '((default (:inherit context-coloring-depth--1-face :slant italic)))
  "Nested blocks face, depth -1; italic; comments."
  :group 'context-coloring-faces)

(defface context-coloring-depth-0-bold-face
  '((default (:inherit context-coloring-depth-0-face :weight bold)))
  "Nested blocks face, depth 0; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-1-bold-face
  '((default (:inherit context-coloring-depth-1-face :weight bold)))
  "Nested blocks face, depth 1; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-2-bold-face
  '((default (:inherit context-coloring-depth-2-face :weight bold)))
  "Nested blocks face, depth 2; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-3-bold-face
  '((default (:inherit context-coloring-depth-3-face :weight bold)))
  "Nested blocks face, depth 3; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-4-bold-face
  '((default (:inherit context-coloring-depth-4-face :weight bold)))
  "Nested blocks face, depth 4; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-5-bold-face
  '((default (:inherit context-coloring-depth-5-face :weight bold)))
  "Nested blocks face, depth 5; bold; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-depth-6-bold-face
  '((default (:inherit context-coloring-depth-6-face :weight bold)))
  "Nested blocks face, depth 6; bold; global scope."
  :group 'context-coloring-faces)


;;; Face utility functions

(defun context-coloring-level-face (depth style)
  "Return face-name for DEPTH as a string \"context-coloring-depth-DEPTH-face\".
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


;;; Constants

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defconst context-coloring-scopifier-path
  (expand-file-name "./bin/scopifier" context-coloring-path)
  "Path to the external scopifier executable.")

(defconst context-coloring-delay 0.25
  "Time between colorization.")


;;; Tokenization functions

;; Tokens are vectors with the following form:
;; 0: Start
;; 1: End
;; 2: Level
;; 3: Style
(defun context-coloring-apply-tokens (tokens)
  "Processes TOKENS to apply context-based coloring to the current buffer."
  (with-silent-modifications
    ;; Reset in case there should be uncolored areas.
    (remove-text-properties (point-min) (point-max) `(face nil rear-nonsticky nil))
    (let ((i 0)
          (len (length tokens)))
      (while (< i len)
        (let ((token (elt tokens i)))
          (let ((start (elt token 0))
                (end (elt token 1))
                (face (context-coloring-level-face (elt token 2) (elt token 3))))
            (add-text-properties start end `(face ,face rear-nonsticky t))))
        (setq i (+ i 1))))))

(defun context-coloring-kill-scopifier ()
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-scopify ()
  "Invokes the external scopifier with the current buffer's
contents, reading the scopifier's response asynchronously and
calling FUNCTION with the parsed list of tokens."

  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-kill-scopifier)

  ;; Start the process.
  (setq context-coloring-scopifier-process
        (start-process-shell-command "scopifier" nil context-coloring-scopifier-path))

  (let ((output "")
        (buffer context-coloring-buffer)
        (start-time context-coloring-colorize-start-time))

    ;;The process may produce output in multiple chunks. This filter accumulates
    ;;the chunks into a message.
    (set-process-filter context-coloring-scopifier-process
                        (lambda (process chunk)
                          (setq output (concat output chunk))))

    ;; When the process's message is complete, this sentinel parses it as JSON
    ;; and applies the tokens to the buffer.
    (set-process-sentinel context-coloring-scopifier-process
                          (lambda (process event)
                            (when (equal "finished\n" event)
                              (let ((tokens (json-read-from-string output)))
                                (with-current-buffer buffer
                                  (context-coloring-apply-tokens tokens))
                                (setq context-coloring-scopifier-process nil)
                                (message "Colorized (after %f seconds)." (- (float-time) start-time))
                                )))))

  ;; Give the process its input.
  (process-send-region context-coloring-scopifier-process (point-min) (point-max))
  (process-send-eof context-coloring-scopifier-process))


;;; Colorization functions

(defun context-coloring-colorize ()
  (interactive)
  (setq context-coloring-colorize-start-time (float-time))
  (message "%s" "Colorizing.")
  (context-coloring-scopify))

(defun context-coloring-change-function (start end length)
  ;; Tokenization is obsolete if there was a change.
  (context-coloring-kill-scopifier)
  (setq context-coloring-changed t))

(defun context-coloring-maybe-colorize ()
  "Colorize under certain conditions. This will run as an idle
timer, so firstly the buffer must not be some other
buffer. Additionally, the buffer must have changed, otherwise
colorizing would be redundant."
  (when (and (eq context-coloring-buffer (window-buffer (selected-window)))
             context-coloring-changed)
    (setq context-coloring-changed nil)
    (context-coloring-colorize)))


;;; Local variables

(defvar context-coloring-buffer nil
  "Reference to this buffer for timers.")
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

(defvar context-coloring-colorize-start-time nil
  "Used for dirty benchmarking of async colorization time.")
(make-variable-buffer-local 'context-coloring-colorize-start-time)


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

    ;; Only recolor on change. So watch for changes.
    (font-lock-mode 0)
    (jit-lock-mode nil)
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
