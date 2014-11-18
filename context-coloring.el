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

;; Probably useful, here's the rainbow-blocks colorizer.
(defsubst rainbow-blocks-propertize-delimiter (loc depth)
  "Highlight a single delimiter at LOC according to DEPTH.
LOC is the location of the character to add text properties to.
DEPTH is the nested depth at LOC, which determines the face to use.
Sets text properties:
`font-lock-face' to the appropriate delimiter face.
`rear-nonsticky' to prevent color from bleeding into subsequent characters typed by the user."
  (with-silent-modifications
    (let* ((delim-face (if (<= depth 0)
                           'rainbow-blocks-unmatched-face
                         (rainbow-blocks-depth-face depth)))
           (end-pos    (save-excursion (goto-char loc)
                                       (forward-sexp)
                                       (point))))
      (add-text-properties loc end-pos
                           `(font-lock-face ,delim-face
                                            rear-nonsticky t)))))
