;;; context-coloring.el --- Syntax highlighting, except not for syntax. -*- lexical-binding: t; -*-

;; Copyright (C) 2014 Jackson Ray Hamilton

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Keywords: context coloring syntax highlighting
;; Version: 1.0.0
;; Package-Requires: ((emacs "24") (js2-mode "20141118"))

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

;; Colors code by scope, rather than by syntax.

;; A range of characters encompassing a scope is colored according to its level;
;; the global scope is white, scopes within the global scope are yellow, scopes
;; within scopes within the global scope are green, etc.  Variables defined in a
;; parent scope which are referenced from child scopes retain the same color as
;; the scope in which they are defined; a variable defined in the global scope
;; will be the same color when referenced from nested scopes.

;; To use, add the following to your ~/.emacs:

;; (require 'context-coloring)
;; (add-hook 'js-mode-hook 'context-coloring-mode) ; Requires Node.js 0.10+.

;;; Code:

(require 'js2-mode)


;;; Constants

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")


;;; Customizable options

(defcustom context-coloring-delay 0.25
  "Delay between a buffer update and colorization.

Increase this if your machine is high-performing. Decrease it if it ain't."
  :group 'context-coloring)

(defcustom context-coloring-block-scopes nil
  "If non-nil, add block scopes to the scope hierarchy.

The block-scope-inducing `let' and `const' are introduced in
ES6. If you are writing ES6 code, then turn this on; otherwise,
confusion will ensue."
  :group 'context-coloring)


;;; Local variables

(defvar-local context-coloring-buffer nil
  "Reference to this buffer (for timers).")

(defvar-local context-coloring-scopifier-process nil
  "Only allow a single scopifier process to run at a time. This
is a reference to that one process.")

(defvar-local context-coloring-colorize-idle-timer nil
  "Reference to currently-running idle timer.")

(defvar-local context-coloring-changed nil
  "Indication that the buffer has changed recently, which would
imply that it should be colorized again.")


;;; Faces

(defface context-coloring-level--1-face
  '((((type tty)) (:foreground "white"))
    (t (:foreground "#7f7f7f")))
  "Context coloring face, level -1; comments."
  :group 'context-coloring-faces)

(defface context-coloring-level-0-face
  '((((type tty)) (:foreground "white"))
    (((background light)) (:foreground "#000000"))
    (((background dark)) (:foreground "#ffffff")))
  "Context coloring face, level 0; global scope."
  :group 'context-coloring-faces)

(defface context-coloring-level-1-face
  '((((type tty)) (:foreground "yellow"))
    (((background light)) (:foreground "#007f80"))
    (((background dark)) (:foreground "#ffff80")))
  "Context coloring face, level 1."
  :group 'context-coloring-faces)

(defface context-coloring-level-2-face
  '((((type tty)) (:foreground "green"))
    (((background light)) (:foreground "#001580"))
    (((background dark)) (:foreground "#cdfacd")))
  "Context coloring face, level 2."
  :group 'context-coloring-faces)

(defface context-coloring-level-3-face
  '((((type tty)) (:foreground "cyan"))
    (((background light)) (:foreground "#550080"))
    (((background dark)) (:foreground "#d8d8ff")))
  "Context coloring face, level 3."
  :group 'context-coloring-faces)

(defface context-coloring-level-4-face
  '((((type tty)) (:foreground "blue"))
    (((background light)) (:foreground "#802b00"))
    (((background dark)) (:foreground "#e7c7ff")))
  "Context coloring face, level 4."
  :group 'context-coloring-faces)

(defface context-coloring-level-5-face
  '((((type tty)) (:foreground "magenta"))
    (((background light)) (:foreground "#6a8000"))
    (((background dark)) (:foreground "#ffcdcd")))
  "Context coloring face, level 5."
  :group 'context-coloring-faces)

(defface context-coloring-level-6-face
  '((((type tty)) (:foreground "red"))
    (((background light)) (:foreground "#008000"))
    (((background dark)) (:foreground "#ffe390")))
  "Context coloring face, level 6."
  :group 'context-coloring-faces)

;;; Additional 6 faces for insane levels of nesting

(defface context-coloring-level-7-face
  '((t (:inherit context-coloring-level-1-face)))
  "Context coloring face, level 7."
  :group 'context-coloring-faces)

(defface context-coloring-level-8-face
  '((t (:inherit context-coloring-level-2-face)))
  "Context coloring face, level 8."
  :group 'context-coloring-faces)

(defface context-coloring-level-9-face
  '((t (:inherit context-coloring-level-3-face)))
  "Context coloring face, level 9."
  :group 'context-coloring-faces)

(defface context-coloring-level-10-face
  '((t (:inherit context-coloring-level-4-face)))
  "Context coloring face, level 10."
  :group 'context-coloring-faces)

(defface context-coloring-level-11-face
  '((t (:inherit context-coloring-level-5-face)))
  "Context coloring face, level 11."
  :group 'context-coloring-faces)

(defface context-coloring-level-12-face
  '((t (:inherit context-coloring-level-6-face)))
  "Context coloring face, level 12."
  :group 'context-coloring-faces)

(defcustom context-coloring-face-count 7
  "Number of faces defined for highlighting delimiter levels.
Determines level at which to cycle through faces again."
  :group 'context-coloring)


;;; Face functions

(defsubst context-coloring-level-face (level)
  "Return face-name for LEVEL as a string \"context-coloring-level-LEVEL-face\".
For example: \"context-coloring-level-1-face\"."
  (intern-soft
   (concat "context-coloring-level-"
           (number-to-string
            (or
             ;; Has a face directly mapping to it.
             (and (< level context-coloring-face-count)
                  level)
             ;; After the number of available faces are used up, pretend the 0th
             ;; face doesn't exist.
             (+ 1
                (mod (- level 1)
                     (- context-coloring-face-count 1)))))
           "-face")))


;;; Colorization utilities

(defun context-coloring-uncolorize-buffer ()
  "Clears all coloring in the current buffer."
  (remove-text-properties (point-min) (point-max) `(face nil rear-nonsticky nil)))

(defsubst context-coloring-colorize-region (start end level)
  "Colorizes characters from 1-indexed START (inclusive) to END
\(exclusive) with the face corresponding to LEVEL."
  (add-text-properties
   start
   end
   `(face ,(context-coloring-level-face level) rear-nonsticky t)))


;;; js2-mode colorization

(defsubst context-coloring-js2-scope-level (scope)
  "Gets the level of SCOPE."
  (let ((level 0)
        enclosing-scope)
    (while (and scope
                (js2-node-parent scope)
                (setq enclosing-scope (js2-node-get-enclosing-scope scope)))
      (when (or context-coloring-block-scopes
                (let ((type (js2-scope-type scope)))
                  (or (= type js2-SCRIPT)
                      (= type js2-FUNCTION)
                      (= type js2-CATCH)
                      (= type js2-WITH))))
        (setq level (+ level 1)))
      (setq scope enclosing-scope))
    level))

;; Adapted from js2-refactor.el/js2r-vars.el
(defsubst context-coloring-js2-local-name-node-p (node)
  (and (js2-name-node-p node)
       (let ((start (js2-node-abs-pos node)))
         (and
          ;; (save-excursion ; not key in object literal { key: value }
          ;;   (goto-char (+ (js2-node-abs-pos node) (js2-node-len node)))
          ;;   (looking-at "[\n\t ]*:"))
          (let ((end (+ start (js2-node-len node))))
            (not (string-match "[\n\t ]*:" (buffer-substring-no-properties
                                            end
                                            (+ end 1)))))
          ;; (save-excursion ; not property lookup on object
          ;;   (goto-char (js2-node-abs-pos node))
          ;;   (looking-back "\\.[\n\t ]*"))
          (not (string-match "\\.[\n\t ]*" (buffer-substring-no-properties
                                            (max 1 (- start 1)) ; 0 throws an
                                                                ; error. "" will
                                                                ; fail the test.
                                            start)))))))

(defsubst context-coloring-js2-colorize-node (node level)
  (let ((start (js2-node-abs-pos node)))
    (context-coloring-colorize-region
     start
     (+ start (js2-node-len node)) ; End
     level)))

(defun context-coloring-js2-colorize ()
  (with-silent-modifications
    ;; (context-coloring-uncolorize-buffer)
    (js2-visit-ast
     js2-mode-ast
     (lambda (node end-p)
       (when (null end-p)
         (cond
          ((js2-comment-node-p node)
           (context-coloring-js2-colorize-node
            node
            -1))
          ((js2-scope-p node)
           (context-coloring-js2-colorize-node
            node
            (context-coloring-js2-scope-level node)))
          ((context-coloring-js2-local-name-node-p node)
           (context-coloring-js2-colorize-node
            node
            (context-coloring-js2-scope-level
             (js2-get-defining-scope
              (js2-node-get-enclosing-scope node)
              (js2-name-node-name node))))))
         ;; The `t' indicates to search children.
         t)))))


;;; Shell command copification / colorization

(defun context-coloring-apply-tokens (tokens)
  "Processes a vector of TOKENS to apply context-based coloring
to the current buffer. Tokens are 3 integers: start, end,
level. The vector is flat, with a new token occurring after every
3rd element."
  (with-silent-modifications
    ;; (context-coloring-uncolorize-buffer)
    (let ((i 0)
          (len (length tokens)))
      (while (< i len)
        (context-coloring-colorize-region
         (elt tokens i)
         (elt tokens (+ i 1))
         (elt tokens (+ i 2)))
        (setq i (+ i 3))))))

(defun context-coloring-parse-array (input)
  "Specialized JSON parser for a flat array of numbers."
  (vconcat (mapcar 'string-to-number (split-string (substring input 1 -1) ","))))

(defun context-coloring-kill-scopifier ()
  "Kills the currently-running scopifier process for this
buffer."
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-scopify-shell-command (command)
  "Invokes a scopifier with the current buffer's contents,
reading the scopifier's response asynchronously and applying a
parsed list of tokens to `context-coloring-apply-tokens'."

  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-kill-scopifier)

  ;; Start the process.
  (setq context-coloring-scopifier-process
        (start-process-shell-command "scopifier" nil command))

  (let ((output "")
        (buffer context-coloring-buffer))

    ;; The process may produce output in multiple chunks. This filter
    ;; accumulates the chunks into a message.
    (set-process-filter
     context-coloring-scopifier-process
     (lambda (_process chunk)
       (setq output (concat output chunk))))

    ;; When the process's message is complete, this sentinel parses it as JSON
    ;; and applies the tokens to the buffer.
    (set-process-sentinel
     context-coloring-scopifier-process
     (lambda (_process event)
       (when (equal "finished\n" event)
         (let ((tokens (context-coloring-parse-array output)))
           (with-current-buffer buffer
             (context-coloring-apply-tokens tokens))
           (setq context-coloring-scopifier-process nil))))))

  ;; Give the process its input so it can begin.
  (process-send-region context-coloring-scopifier-process (point-min) (point-max))
  (process-send-eof context-coloring-scopifier-process))


;;; Dispatch

(defvar context-coloring-javascript-scopifier
  `(:type shell-command
          :executable "node"
          :command ,(expand-file-name
                     "./languages/javascript/bin/scopifier"
                     context-coloring-path)))

(defvar context-coloring-js2-colorizer
  `(:type elisp
          :colorizer context-coloring-js2-colorize))

(defcustom context-coloring-dispatch-plist
  `(js-mode ,context-coloring-javascript-scopifier
            js2-mode ,context-coloring-js2-colorizer
            js3-mode ,context-coloring-javascript-scopifier)
  "Property list mapping major modes to scopification programs."
  :group 'context-coloring)

(defun context-coloring-dispatch ()
  "Determines the optimal track for scopification / colorization
of the current buffer, then does it."
  (let ((dispatch (plist-get context-coloring-dispatch-plist major-mode)))
    (if (null dispatch)
        (message "%s" "Context coloring is not available for this major mode"))
    (let ((type (plist-get dispatch :type)))
      (cond
       ((eq type 'elisp)
        (let ((colorizer (plist-get dispatch :colorizer))
              (scopifier (plist-get dispatch :scopifier)))
          (cond
           ((not (null colorizer))
            (funcall colorizer))
           ((not (null scopifier))
            (context-coloring-apply-tokens (funcall scopifier)))
           (t
            (error "No `:colorizer' nor `:scopifier' specified for dispatch of `:type' elisp")))))
       ((eq type 'shell-command)
        (let ((executable (plist-get dispatch :executable))
              (command (plist-get dispatch :command)))
          (if (null command)
              (error "No `:command' specified for dispatch of `:type' shell-command"))
          (if (and (not (null executable))
                   (null (executable-find executable)))
              (message "Executable \"%s\" not found" executable))
          (context-coloring-scopify-shell-command command)))))))


;;; Colorization

(defun context-coloring-colorize ()
  "Colors the current buffer by function context."
  (interactive)
  (context-coloring-dispatch))

(defun context-coloring-change-function (_start _end _length)
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
  "Context-based code coloring, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        (context-coloring-kill-scopifier)
        (when (not (null 'context-coloring-colorize-idle-timer))
          (cancel-timer context-coloring-colorize-idle-timer))
        (remove-hook 'js2-post-parse-callbacks 'context-coloring-change-function t)
        (remove-hook 'after-change-functions 'context-coloring-change-function t)
        (font-lock-mode)
        (jit-lock-mode t))

    ;; Remember this buffer. This value should not be dynamically-bound.
    (setq context-coloring-buffer (current-buffer))

    ;; Font lock is incompatible with this mode; the converse is also true.
    (font-lock-mode 0)
    (jit-lock-mode nil)

    ;; Colorize once initially.
    ;; (let ((start-time (float-time)))
    (context-coloring-colorize)
    ;;  (message "Elapsed time: %f" (- (float-time) start-time)))

    (cond
     ((equal major-mode 'js2-mode)
      ;; Only recolor on reparse.
      (add-hook 'js2-post-parse-callbacks 'context-coloring-colorize nil t))
     (t
      ;; Only recolor on change.
      (add-hook 'after-change-functions 'context-coloring-change-function nil t)))

    (when (not (equal major-mode 'js2-mode))
      ;; Only recolor idly.
      (setq context-coloring-colorize-idle-timer
            (run-with-idle-timer context-coloring-delay t 'context-coloring-maybe-colorize)))))

(provide 'context-coloring)

;;; context-coloring.el ends here
