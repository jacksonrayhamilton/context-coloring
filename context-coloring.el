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
