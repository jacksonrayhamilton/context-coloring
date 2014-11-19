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
  "Return face-name for DEPTH as a string 'context-coloring-depth-DEPTH-face'.
For example: 'context-coloring-depth-1-face'."
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


;;; The coloring

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defconst context-coloring-tokenizer-path
  (expand-file-name "./bin/tokenizer" context-coloring-path))

(defun context-coloring-apply-tokens (tokens)
  (with-silent-modifications
    (dolist (token tokens)
      (let ((start (cdr (assoc 's token)))
            (end (cdr (assoc 'e token)))
            (face (context-coloring-level-face (cdr (assoc 'l token)))))
        (add-text-properties start end `(font-lock-face ,face rear-nonsticky t))))))

(defun context-coloring-tokenizer-filter (process chunk)
  (setq context-coloring-tokenizer-output
        (concat context-coloring-tokenizer-output chunk)))

(defun context-coloring-tokenizer-sentinel (process event)
  (when (equal "finished\n" event)
    (let ((tokens (let ((json-array-type 'list))
                    (json-read-from-string context-coloring-tokenizer-output))))
      (setq context-coloring-tokenizer-output nil)
      (context-coloring-apply-tokens tokens))))

(defun context-coloring-tokenize ()
  ;; Only continue if there is no concurrent tokenization going on.
  (when (eq context-coloring-tokenizer-output nil)
    (let ((tokenizer-process (start-process-shell-command
                              "tokenizer"
                              nil
                              context-coloring-tokenizer-path)))
      (setq context-coloring-tokenizer-output "")
      (set-process-filter tokenizer-process 'context-coloring-tokenizer-filter)
      (set-process-sentinel tokenizer-process 'context-coloring-tokenizer-sentinel)
      (process-send-region tokenizer-process (point-min) (point-max))
      (process-send-eof tokenizer-process))))

(defun context-coloring-colorize-buffer ()
  (interactive)
  (context-coloring-tokenize))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring for JavaScript, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        )
    (set (make-local-variable 'context-coloring-tokenizer-output) nil)))

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
