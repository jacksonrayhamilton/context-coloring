;; Musings:

;; Each time a function statement/expression is encountered, a new scope is
;; created. New scopes use the next unique color. Multiple scopes within the
;; same scope have the same color.

;; function statement syntax:
;;
;; function name([param1[, param2[, ..., paramN]]]) { [statements] }

;; A function statement's name is added to the previous scope and is colored in
;; the previous scope's color. The rest of the function is part of the new
;; scope.

;; function expression syntax:
;;
;; function [name]([param1[, param2[, ..., paramN]]]) { [statements] }

;; A function expression's name, if it has one, is part of the new scope created
;; by the expression.


;; JSLINT strategy:

;; Colors for levels:

;; white
;; #ffff80
;; #cdfacd
;; #d8d8ff
;; #e7c7ff
;; #ffcdcd
;; #ffe390
;; #cdcdcd

;; - Obtain the buffer's contents, pipe them to a JSLint server.
;; - Run code through JSLINT.
;; - Pipe `JSLINT.data().tokens` back to emacs.
;; - Iterate through the tokens.
;;   - On line `line`, from `from` through `thru` inclusive, color the area
;;   according to `function.level`
;; - ...
;; - Profit.

;; elisp functions that may come in handy:

;; save-excursion: Wrap the whole body in this.
;; goto-line, move-to-column: Finding ranges to apply colors to.
;; with-silent-modifications: The colorization itself.

(require 'json)

;; Faces for highlighting blocks by nested level:

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

(defun context-coloring-depth-face (depth)
  "Return face-name for DEPTH as a string 'context-coloring-depth-DEPTH-face'.
For example: 'context-coloring-depth-1-face'."
  (intern-soft
   (concat "context-coloring-depth-"
           (number-to-string (mod depth context-coloring-face-count))
           "-face")))

(defconst context-coloring-path
  (file-name-directory (or load-file-name buffer-file-name)))

(defun context-coloring-get-point (line column)
  (save-excursion
    (goto-line line)
    (move-to-column column)
    (point)))

(defun context-coloring ()
  (interactive)
  (let* ((json (shell-command-to-string
                 (format "echo '%s' | %s"
                         (buffer-substring-no-properties
                          (point-min)
                          (point-max))
                         (expand-file-name "./tokenizer/tokenizer" context-coloring-path))))
         (tokens (let ((json-array-type 'list))
                   (json-read-from-string json))))
    (with-silent-modifications
      (dolist (token tokens)
        (let* ((line (cdr (assoc 'line token)))
               (from (cdr (assoc 'from token)))
               (thru (cdr (assoc 'thru token)))
               (level (cdr (assoc 'level token)))
               (start (context-coloring-get-point line (- from 1)))
               (end (context-coloring-get-point line (- thru 1)))
               (face (context-coloring-depth-face level)))
          (message "from %s to %s, use face %s" start end face)
          (add-text-properties start end `(font-lock-face ,face rear-nonsticky t)))))))
