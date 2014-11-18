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

(defsubst context-coloring-level-face (depth)
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

(defun context-coloring-save-buffer-to-temp ()
  "Save buffer to temp file.
Return the name of the temporary file."
  (let ((file-name (make-temp-file "context-coloring")))
    ;; Do not flush short-lived temporary files onto disk.
    (let ((write-region-inhibit-fsync t))
      (write-region nil nil file-name nil 0))
    file-name))

;;; The coloring.

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-fontify-region (start end)
  (interactive)
  (let* ((temp-file (context-coloring-save-buffer-to-temp))
         (json (shell-command-to-string
                (format "%s < %s"
                        (expand-file-name "./bin/tokenizer" context-coloring-path)
                        temp-file)))
         (tokens (let ((json-array-type 'list))
                   (json-read-from-string json))))
    (with-silent-modifications
      (dolist (token tokens)
        (let ((s (cdr (assoc 's token)))
              (e (cdr (assoc 'e token)))
              (face (context-coloring-level-face (cdr (assoc 'l token)))))
          (when (and (>= s start)
                     (<= e end))
            (add-text-properties s e `(font-lock-face ,face rear-nonsticky t))))))
    (delete-file temp-file)))

;;; Minor mode:

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring for JavaScript, inspired by Douglas Crockford."
  nil " Context" nil
  (make-local-variable 'jit-lock-stealth-time)
  (make-local-variable 'jit-lock-chunk-size)
  (make-local-variable 'jit-lock-contextually)
  (if (not context-coloring-mode)
      (progn
        (setq jit-lock-stealth-time nil)
        (setq jit-lock-chunk-size 500)
        ;;(setq jit-lock-contextually `syntax-driven)
        (jit-lock-unregister 'context-coloring-fontify-region)
        (jit-lock-register 'font-lock-fontify-region))
    (setq jit-lock-stealth-time 1)
    (setq jit-lock-chunk-size 536870911)
    ;;(setq jit-lock-contextually nil)
    (jit-lock-unregister 'font-lock-fontify-region)
    (jit-lock-register 'context-coloring-fontify-region t)))

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
