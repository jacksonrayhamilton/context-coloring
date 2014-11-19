;; -*- lexical-binding: t -*-

(require 'json)

;;; Faces

(defface context-coloring-depth-0-face
  '((((background light)) (:foreground "#ffffff"))
    (((background dark)) (:foreground "#ffffff")))
  "Nested blocks face, depth 0 - outermost set."
  :tag "Rainbow Blocks Depth 0 Face -- OUTERMOST"
  :group 'context-coloring-faces)

(defface context-coloring-depth-1-face
  '((((background light)) (:foreground "#ffff80"))
    (((background dark)) (:foreground "#ffff80")))
  "Nested blocks face, depth 1."
  :group 'context-coloring-faces)

(defface context-coloring-depth-2-face
  '((((background light)) (:foreground "#cdfacd"))
    (((background dark)) (:foreground "#cdfacd")))
  "Nested blocks face, depth 2."
  :group 'context-coloring-faces)

(defface context-coloring-depth-3-face
  '((((background light)) (:foreground "#d8d8ff"))
    (((background dark)) (:foreground "#d8d8ff")))
  "Nested blocks face, depth 3."
  :group 'context-coloring-faces)

(defface context-coloring-depth-4-face
  '((((background light)) (:foreground "#e7c7ff"))
    (((background dark)) (:foreground "#e7c7ff")))
  "Nested blocks face, depth 4."
  :group 'context-coloring-faces)

(defface context-coloring-depth-5-face
  '((((background light)) (:foreground "#ffcdcd"))
    (((background dark)) (:foreground "#ffcdcd")))
  "Nested blocks face, depth 5."
  :group 'context-coloring-faces)

(defface context-coloring-depth-6-face
  '((((background light)) (:foreground "#ffe390"))
    (((background dark)) (:foreground "#ffe390")))
  "Nested blocks face, depth 6."
  :group 'context-coloring-faces)

(defface context-coloring-depth-7-face
  '((((background light)) (:foreground "#cdcdcd"))
    (((background dark)) (:foreground "#cdcdcd")))
  "Nested blocks face, depth 7."
  :group 'context-coloring-faces)

(defconst context-coloring-face-count 8
  "Number of faces defined for highlighting delimiter levels.
Determines depth at which to cycle through faces again.")


;;; Face utility functions

(defun context-coloring-level-face (depth)
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
           "-face")))


;;; Path constants

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name))
  "This file's directory.")

(defconst context-coloring-tokenizer-path
  (expand-file-name "./bin/tokenizer" context-coloring-path)
  "Path to the external tokenizer executable.")


;;; Tokenization functions

(defun context-coloring-apply-tokens (tokens)
  "Processes TOKENS to apply context-based coloring to the current buffer."
  (with-silent-modifications
    (dolist (token tokens)
      (let ((start (cdr (assoc 's token)))
            (end (cdr (assoc 'e token)))
            (face (context-coloring-level-face (cdr (assoc 'l token)))))
        (add-text-properties start end `(font-lock-face ,face rear-nonsticky t))))))

(defun context-coloring-tokenize (function)
  "Invokes the external tokenizer with the current buffer's
contents, reading the tokenizer's response asynchronously and
calling FUNCTION with the parsed list of tokens."
  (let ((tokenizer-process (start-process-shell-command
                            "tokenizer"
                            nil
                            context-coloring-tokenizer-path))
        (output ""))

    ;; The process may produce output in multiple chunks. The chunks
    ;; collectively form a message.
    (set-process-filter tokenizer-process
                        (lambda (process chunk)
                          (setq output (concat output chunk))))

    ;; When the message is complete, parse it as JSON and apply the tokens.
    (set-process-sentinel tokenizer-process
                          (lambda (process event)
                            (when (equal "finished\n" event)
                              (let ((tokens (let ((json-array-type 'list))
                                              (json-read-from-string output))))
                                (funcall function tokens)))))

    ;; Give the process its input.
    (process-send-region tokenizer-process (point-min) (point-max))
    (process-send-eof tokenizer-process)))


;;; Colorization functions

(defun context-coloring-colorize-buffer ()
  (interactive)
  (context-coloring-tokenize 'context-coloring-apply-tokens))

(defun context-coloring-colorize-if-current ()
  (when (eq context-coloring-buffer (window-buffer (selected-window)))
    (context-coloring-colorize-buffer)))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring for JavaScript, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        (when (boundp 'context-coloring-colorize-idle-timer)
         (cancel-timer context-coloring-colorize-idle-timer)))

    ;; Colorize once initially. Why this doesn't work, I cannot say.
    ;; (context-coloring-colorize-buffer)

    ;; Preserve a reference to this buffer.
    (set (make-local-variable 'context-coloring-buffer) (current-buffer))

    ;; Only recolor idly.
    (set (make-local-variable 'context-coloring-colorize-idle-timer)
         (run-with-idle-timer 1 t 'context-coloring-colorize-if-current))))

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
