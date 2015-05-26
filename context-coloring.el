;;; context-coloring.el --- Highlight by scope  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 6.3.0
;; Keywords: convenience faces tools
;; Package-Requires: ((emacs "24") (js2-mode "20150126"))
;; URL: https://github.com/jacksonrayhamilton/context-coloring

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

;; Highlights code by scope.  Top-level scopes are one color, second-level
;; scopes are another color, and so on.  Variables retain the color of the scope
;; in which they are defined.  A variable defined in an outer scope referenced
;; by an inner scope is colored the same as the outer scope.

;; By default, comments and strings are still highlighted syntactically.

;;; Code:

(require 'js2-mode)


;;; Utilities

(defun context-coloring-join (strings delimiter)
  "Join a list of STRINGS with the string DELIMITER."
  (mapconcat 'identity strings delimiter))

(defsubst context-coloring-trim-right (string)
  "Remove leading whitespace from STRING."
  (if (string-match "[ \t\n\r]+\\'" string)
      (replace-match "" t t string)
    string))

(defsubst context-coloring-trim-left (string)
  "Remove trailing whitespace from STRING."
  (if (string-match "\\`[ \t\n\r]+" string)
      (replace-match "" t t string)
    string))

(defsubst context-coloring-trim (string)
  "Remove leading and trailing whitespace from STRING."
  (context-coloring-trim-left (context-coloring-trim-right string)))


;;; Faces

(defun context-coloring-defface (level tty light dark)
  "Define a face for LEVEL with colors for TTY, LIGHT and DARK
backgrounds."
  (let ((face (intern (format "context-coloring-level-%s-face" level)))
        (doc (format "Context coloring face, level %s." level)))
    (custom-declare-face
     face
     `((((type tty)) (:foreground ,tty))
       (((background light)) (:foreground ,light))
       (((background dark)) (:foreground ,dark)))
     doc
     :group 'context-coloring)))

(defun context-coloring-defface-neutral (level)
  "Define a face for LEVEL with the default neutral colors."
  (context-coloring-defface level nil "#3f3f3f" "#cdcdcd"))

(context-coloring-defface 0 nil       "#000000" "#ffffff")
(context-coloring-defface 1 "yellow"  "#008b8b" "#00ffff")
(context-coloring-defface 2 "green"   "#0000ff" "#87cefa")
(context-coloring-defface 3 "cyan"    "#483d8b" "#b0c4de")
(context-coloring-defface 4 "blue"    "#a020f0" "#eedd82")
(context-coloring-defface 5 "magenta" "#a0522d" "#98fb98")
(context-coloring-defface 6 "red"     "#228b22" "#7fffd4")
(context-coloring-defface-neutral 7)

(defvar context-coloring-maximum-face nil
  "Index of the highest face available for coloring.")

(defvar context-coloring-original-maximum-face nil
  "Fallback value for `context-coloring-maximum-face' when all
  themes have been disabled.")

(setq context-coloring-maximum-face 7)

(setq context-coloring-original-maximum-face
      context-coloring-maximum-face)

;; Theme authors can have up to 26 levels: 1 (0th) for globals, 24 (1st-24th)
;; for nested levels, and 1 (25th) for infinity.
(dotimes (number 18)
  (context-coloring-defface-neutral (+ number context-coloring-maximum-face 1)))


;;; Face functions

(defsubst context-coloring-level-face (level)
  "Return the symbol for a face with LEVEL."
  ;; `concat' is faster than `format' here.
  (intern-soft
   (concat "context-coloring-level-" (number-to-string level) "-face")))

(defsubst context-coloring-bounded-level-face (level)
  "Return the symbol for a face with LEVEL, bounded by
`context-coloring-maximum-face'."
  (context-coloring-level-face (min level context-coloring-maximum-face)))


;;; Colorization utilities

(defsubst context-coloring-colorize-region (start end level)
  "Color characters from the 1-indexed START point (inclusive) to
the END point (exclusive) with the face corresponding to LEVEL."
  (add-text-properties
   start
   end
   `(face ,(context-coloring-bounded-level-face level))))

(make-obsolete-variable
 'context-coloring-comments-and-strings
 "use `context-coloring-syntactic-comments' and
 `context-coloring-syntactic-strings' instead."
 "6.1.0")

(defcustom context-coloring-syntactic-comments t
  "If non-nil, also color comments using `font-lock'."
  :group 'context-coloring)

(defcustom context-coloring-syntactic-strings t
  "If non-nil, also color strings using `font-lock'."
  :group 'context-coloring)

(defun context-coloring-font-lock-syntactic-comment-function (state)
  "Tell `font-lock' to color a comment but not a string."
  (if (nth 3 state) nil font-lock-comment-face))

(defun context-coloring-font-lock-syntactic-string-function (state)
  "Tell `font-lock' to color a string but not a comment."
  (if (nth 3 state) font-lock-string-face nil))

(defsubst context-coloring-maybe-colorize-comments-and-strings (&optional min max)
  "Color the current buffer's comments or strings if
`context-coloring-syntactic-comments' or
`context-coloring-syntactic-strings' are non-nil."
  (when (or context-coloring-syntactic-comments
            context-coloring-syntactic-strings)
    (let ((min (or min (point-min)))
          (max (or max (point-max)))
          (font-lock-syntactic-face-function
           (cond
            ((and context-coloring-syntactic-comments
                  (not context-coloring-syntactic-strings))
             'context-coloring-font-lock-syntactic-comment-function)
            ((and context-coloring-syntactic-strings
                  (not context-coloring-syntactic-comments))
             'context-coloring-font-lock-syntactic-string-function)
            (t
             font-lock-syntactic-face-function))))
      (save-excursion
        (font-lock-fontify-syntactically-region min max)
        ;; TODO: Make configurable at the dispatch level.
        (when (eq major-mode 'emacs-lisp-mode)
          (font-lock-fontify-keywords-region min max))))))


;;; js2-mode colorization

(defvar-local context-coloring-js2-scope-level-hash-table nil
  "Associate `js2-scope' structures and with their scope
  levels.")

(defcustom context-coloring-js-block-scopes nil
  "If non-nil, also color block scopes in the scope hierarchy in JavaScript.

The block-scoped `let' and `const' are introduced in ES6.  Enable
this for ES6 code; disable it elsewhere.

Supported modes: `js2-mode'"
  :group 'context-coloring)

(defsubst context-coloring-js2-scope-level (scope)
  "Return the level of SCOPE."
  (cond ((gethash scope context-coloring-js2-scope-level-hash-table))
        (t
         (let ((level 0)
               (current-scope scope)
               enclosing-scope)
           (while (and current-scope
                       (js2-node-parent current-scope)
                       (setq enclosing-scope
                             (js2-node-get-enclosing-scope current-scope)))
             (when (or context-coloring-js-block-scopes
                       (let ((type (js2-scope-type current-scope)))
                         (or (= type js2-SCRIPT)
                             (= type js2-FUNCTION)
                             (= type js2-CATCH))))
               (setq level (+ level 1)))
             (setq current-scope enclosing-scope))
           (puthash scope level context-coloring-js2-scope-level-hash-table)))))

(defsubst context-coloring-js2-local-name-node-p (node)
  "Determine if NODE is a `js2-name-node' representing a local
variable."
  (and (js2-name-node-p node)
       (let ((parent (js2-node-parent node)))
         (not (or (and (js2-object-prop-node-p parent)
                       (eq node (js2-object-prop-node-left parent)))
                  (and (js2-prop-get-node-p parent)
                       ;; For nested property lookup, the node on the left is a
                       ;; `js2-prop-get-node', so this always works.
                       (eq node (js2-prop-get-node-right parent))))))))

(defvar-local context-coloring-point-max nil
  "Cached value of `point-max'.")

(defsubst context-coloring-js2-colorize-node (node level)
  "Color NODE with the color for LEVEL."
  (let ((start (js2-node-abs-pos node)))
    (context-coloring-colorize-region
     start
     (min
      ;; End
      (+ start (js2-node-len node))
      ;; Somes nodes (like the ast when there is an unterminated multiline
      ;; comment) will stretch to the value of `point-max'.
      context-coloring-point-max)
     level)))

(defun context-coloring-js2-colorize ()
  "Color the current buffer using the abstract syntax tree
generated by `js2-mode'."
  ;; Reset the hash table; the old one could be obsolete.
  (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test 'eq))
  (setq context-coloring-point-max (point-max))
  (with-silent-modifications
    (js2-visit-ast
     js2-mode-ast
     (lambda (node end-p)
       (when (null end-p)
         (cond
          ((js2-scope-p node)
           (context-coloring-js2-colorize-node
            node
            (context-coloring-js2-scope-level node)))
          ((context-coloring-js2-local-name-node-p node)
           (let* ((enclosing-scope (js2-node-get-enclosing-scope node))
                  (defining-scope (js2-get-defining-scope
                                   enclosing-scope
                                   (js2-name-node-name node))))
             ;; The tree seems to be walked lexically, so an entire scope will
             ;; be colored, including its name nodes, before they are reached.
             ;; Coloring the nodes defined in that scope would be redundant, so
             ;; don't do it.
             (when (not (eq defining-scope enclosing-scope))
               (context-coloring-js2-colorize-node
                node
                (context-coloring-js2-scope-level defining-scope))))))
         ;; The `t' indicates to search children.
         t)))
    (context-coloring-maybe-colorize-comments-and-strings)))


;;; Emacs Lisp colorization

(defsubst context-coloring-make-scope (depth level)
  (list
   :depth depth
   :level level
   :variables (make-hash-table)))

(defsubst context-coloring-scope-get-level (scope)
  (plist-get scope :level))

(defsubst context-coloring-scope-add-variable (scope variable)
  (puthash variable t (plist-get scope :variables)))

(defsubst context-coloring-scope-get-variable (scope variable)
  (gethash variable (plist-get scope :variables)))

(defsubst context-coloring-get-variable-level (scope-stack variable)
  (let* (scope
         level)
    (while (and scope-stack (not level))
      (setq scope (car scope-stack))
      (cond
       ((context-coloring-scope-get-variable scope variable)
        (setq level (context-coloring-scope-get-level scope)))
       (t
        (setq scope-stack (cdr scope-stack)))))
    ;; Assume a global variable.
    (or level 0)))

(defsubst context-coloring-make-backtick (end enabled)
  (list
   :end end
   :enabled enabled))

(defsubst context-coloring-backtick-get-end (backtick)
  (plist-get backtick :end))

(defsubst context-coloring-backtick-get-enabled (backtick)
  (plist-get backtick :enabled))

(defsubst context-coloring-backtick-enabled-p (backtick-stack)
  (context-coloring-backtick-get-enabled (car backtick-stack)))

(defsubst context-coloring-make-let-varlist (depth type)
  (list
   :depth depth
   :type type
   :vars '()))

(defsubst context-coloring-let-varlist-get-type (let-varlist)
  (plist-get let-varlist :type))

(defsubst context-coloring-let-varlist-add-var (let-varlist var)
  (plist-put let-varlist :vars (cons var (plist-get let-varlist :vars))))

(defsubst context-coloring-let-varlist-pop-vars (let-varlist)
  (let ((type (context-coloring-let-varlist-get-type let-varlist))
        (vars (plist-get let-varlist :vars)))
    (cond
     ;; `let' binds all at once at the end.
     ((eq type 'let)
      (prog1
          vars
        (plist-put let-varlist :vars '())))
     ;; `let*' binds incrementally.
     ((eq type 'let*)
      (prog1
          (list (car vars))
        (plist-put let-varlist :vars (cdr vars)))))))

(defsubst context-coloring-forward-sws ()
  "Move forward through whitespace and comments."
  (while (forward-comment 1)))

(defsubst context-coloring-forward-sexp-position ()
  "Like vanilla `forward-sexp', but just return the position."
  (scan-sexps (point) 1))

(defsubst context-coloring-emacs-lisp-identifier-syntax-p (syntax-code)
  (or (= 2 syntax-code)
      (= 3 syntax-code)))

(defsubst context-coloring-open-parenthesis-p (syntax-code)
  (= 4 syntax-code))

(defsubst context-coloring-close-parenthesis-p (syntax-code)
  (= 5 syntax-code))

(defsubst context-coloring-expression-prefix-p (syntax-code)
  (= 6 syntax-code))

(defsubst context-coloring-at-open-parenthesis-p ()
  (= 4 (logand #xFFFF (car (syntax-after (point))))))

(defsubst context-coloring-ppss-depth (ppss)
  ;; Same as (nth 0 ppss).
  (car ppss))

(defsubst context-coloring-at-stack-depth-p (stack depth)
  (= (plist-get (car stack) :depth) depth))

(defsubst context-coloring-exact-regexp (word)
  "Create a regexp that matches exactly WORD."
  (concat "\\`" (regexp-quote word) "\\'"))

(defsubst context-coloring-exact-or-regexp (words)
  "Create a regexp that matches any exact word in WORDS."
  (context-coloring-join
   (mapcar 'context-coloring-exact-regexp words) "\\|"))

(defconst context-coloring-emacs-lisp-defun-regexp
  (context-coloring-exact-or-regexp
   '("defun" "defun*" "defsubst" "defmacro"
     "cl-defun" "cl-defsubst" "cl-defmacro")))

(defconst context-coloring-emacs-lisp-lambda-regexp
  (context-coloring-exact-regexp "lambda"))

(defconst context-coloring-emacs-lisp-let-regexp
  (context-coloring-exact-regexp "let"))

(defconst context-coloring-emacs-lisp-let*-regexp
  (context-coloring-exact-regexp "let*"))

(defconst context-coloring-emacs-lisp-arglist-arg-regexp
  "\\`[^&:]")

(defconst context-coloring-ignored-word-regexp
  (concat "\\`[&:-+]?[0-9]\\|" (context-coloring-exact-or-regexp
                                '("t" "nil" "." "?"))))

(defconst context-coloring-WORD-CODE 2)
(defconst context-coloring-SYMBOL-CODE 3)
(defconst context-coloring-OPEN-PARENTHESIS-CODE 4)
(defconst context-coloring-CLOSE-PARENTHESIS-CODE 5)
(defconst context-coloring-EXPRESSION-PREFIX-CODE 6)

(defconst context-coloring-APOSTROPHE-CHAR (string-to-char "'"))
(defconst context-coloring-OPEN-PARENTHESIS-CHAR (string-to-char "("))
(defconst context-coloring-COMMA-CHAR (string-to-char ","))
(defconst context-coloring-BACKTICK-CHAR (string-to-char "`"))

(defvar context-coloring-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.")

(defconst context-coloring-emacs-lisp-iterations-per-pause 1000
  "Pause after this many iterations to check for user input.
If user input is pending, stop the parse.  This makes for a
smoother user experience for large files.

As of this writing, emacs lisp colorization seems to run at about
60,000 iterations per second.  A default value of 1000 should
provide visually \"instant\" updates at 60 frames per second.")

(defvar context-coloring-elisp-scope-stack '())

(defsubst context-coloring-elisp-make-scope (level)
  (list
   :level level
   :variables (make-hash-table :test 'equal)))

(defsubst context-coloring-elisp-scope-get-level (scope)
  (plist-get scope :level))

(defsubst context-coloring-elisp-scope-add-variable (scope variable)
  (puthash variable t (plist-get scope :variables)))

(defsubst context-coloring-elisp-scope-get-variable (scope variable)
  (gethash variable (plist-get scope :variables)))

(defsubst context-coloring-elisp-get-variable-level (variable)
  (let* ((scope-stack context-coloring-elisp-scope-stack)
         scope
         level)
    (while (and scope-stack (not level))
      (setq scope (car scope-stack))
      (cond
       ((context-coloring-elisp-scope-get-variable scope variable)
        (setq level (context-coloring-elisp-scope-get-level scope)))
       (t
        (setq scope-stack (cdr scope-stack)))))
    ;; Assume a global variable.
    (or level 0)))

(defun context-coloring-elisp-push-scope ()
  (push (context-coloring-elisp-make-scope
         (1+ (context-coloring-elisp-current-scope-level)))
        context-coloring-elisp-scope-stack))

(defun context-coloring-elisp-pop-scope ()
  (pop context-coloring-elisp-scope-stack))

(defun context-coloring-elisp-add-variable (variable)
  (let ((current-scope (car context-coloring-elisp-scope-stack)))
    (context-coloring-elisp-scope-add-variable current-scope variable)))

(defun context-coloring-elisp-current-scope-level ()
  (let ((current-scope (car context-coloring-elisp-scope-stack)))
    (cond
     (current-scope
      (context-coloring-elisp-scope-get-level current-scope))
     (t
      0))))

(defun context-coloring-elisp-colorize-defun (&optional anonymous-p)
  (let ((start (point))
        end
        stop
        syntax
        syntax-code
        defun-name-pos
        defun-name-end
        arg-n-pos
        arg-n-end
        arg-n-string)
    (context-coloring-elisp-push-scope)
    ;; Color the whole sexp.
    (forward-sexp)
    (setq end (point))
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-current-scope-level))
    (goto-char start)
    ;; Skip past the "defun".
    (skip-syntax-forward "^w_")
    (forward-sexp)
    (skip-syntax-forward " ")
    (setq stop nil)
    (unless anonymous-p
      ;; Check for the defun's name.
      (setq syntax (syntax-after (point)))
      (setq syntax-code (syntax-class syntax))
      (cond
       ((or (= syntax-code context-coloring-WORD-CODE)
            (= syntax-code context-coloring-SYMBOL-CODE))
        ;; Color the defun's name with the top-level color.
        (setq defun-name-pos (point))
        (forward-sexp)
        (setq defun-name-end (point))
        (context-coloring-colorize-region defun-name-pos defun-name-end 0)
        (skip-syntax-forward " "))
       (t
        (setq stop t))))
    (cond
     (stop
      ;; Skip it.
      (goto-char start)
      (forward-sexp))
     (t
      (setq syntax (syntax-after (point)))
      (setq syntax-code (syntax-class syntax))
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (forward-char)
        (skip-syntax-forward " ")
        (while (/= (progn
                     (setq syntax (syntax-after (point)))
                     (setq syntax-code (syntax-class syntax))
                     syntax-code)
                   context-coloring-CLOSE-PARENTHESIS-CODE)
          (cond
           ((or (= syntax-code context-coloring-WORD-CODE)
                (= syntax-code context-coloring-SYMBOL-CODE))
            (setq arg-n-pos (point))
            (forward-sexp)
            (setq arg-n-end (point))
            (setq arg-n-string (buffer-substring-no-properties
                                arg-n-pos
                                arg-n-end))
            (when (string-match-p
                   context-coloring-emacs-lisp-arglist-arg-regexp
                   arg-n-string)
              (context-coloring-elisp-add-variable arg-n-string)))
           (t
            (forward-sexp)))
          (skip-syntax-forward " "))
        ;; Skip the closing arglist paren.
        (forward-char)
        ;; Colorize the rest of the function.
        (context-coloring-elisp-colorize-region (point) (1- end))
        ;; Exit the defun.
        (forward-char))
       (t
        ;; Skip it.
        (goto-char start)
        (forward-sexp)))))
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-colorize-lambda ()
  (context-coloring-elisp-colorize-defun t))

(defun context-coloring-elisp-colorize-parenthesized-sexp ()
  (let ((start (point))
        end
        syntax
        syntax-code
        child-0-pos
        child-0-end
        child-0-string)
    (forward-sexp)
    (setq end (point))
    (goto-char start)
    (forward-char)
    (skip-syntax-forward " ")
    (setq syntax (syntax-after (point)))
    (setq syntax-code (syntax-class syntax))
    ;; Figure out if the sexp is a special form.
    (cond
     ((or (= syntax-code context-coloring-WORD-CODE)
          (= syntax-code context-coloring-SYMBOL-CODE))
      (setq child-0-pos (point))
      (forward-sexp)
      (setq child-0-end (point))
      (setq child-0-string (buffer-substring-no-properties
                            child-0-pos
                            child-0-end))
      (cond
       ((string-match-p context-coloring-emacs-lisp-defun-regexp child-0-string)
        (goto-char start)
        (context-coloring-elisp-colorize-defun))
       ((string-match-p context-coloring-emacs-lisp-lambda-regexp child-0-string)
        (goto-char start)
        (context-coloring-elisp-colorize-lambda))
       ;; Not a special form; just colorize the remaining region.
       (t
        (context-coloring-colorize-region
         start
         end
         (context-coloring-elisp-current-scope-level))
        (context-coloring-elisp-colorize-region (point) (1- end))
        (forward-char))))
     (t
      ;; Skip it.
      (goto-char start)
      (forward-sexp)))))

(defun context-coloring-elisp-colorize-symbol ()
  (let (symbol-pos
        symbol-end
        symbol-string)
    (setq symbol-pos (point))
    (forward-sexp)
    (setq symbol-end (point))
    (setq symbol-string (buffer-substring-no-properties
                         symbol-pos
                         symbol-end))
    (cond
     ((string-match-p context-coloring-ignored-word-regexp symbol-string))
     (t
      (context-coloring-colorize-region
       symbol-pos
       symbol-end
       (context-coloring-elisp-get-variable-level
        (buffer-substring-no-properties
         symbol-pos
         symbol-end)))))))

(defun context-coloring-elisp-colorize-expression-prefix ()
  (let (start
        end
        char)
    (setq char (char-after))
    (cond
     ((= char context-coloring-APOSTROPHE-CHAR)
      (forward-sexp))
     ((= char context-coloring-BACKTICK-CHAR)
      (setq start (point))
      (forward-sexp)
      (setq end (point))
      (goto-char start)
      (while (> end (progn (forward-char)
                           (point)))
        (setq char (char-after))
        (when (= char context-coloring-COMMA-CHAR)
          (forward-char)
          (skip-syntax-forward " ")
          (context-coloring-elisp-colorize-sexp)))))))

(defun context-coloring-elisp-colorize-sexp ()
  (let (syntax
        syntax-code)
    (setq syntax (syntax-after (point)))
    (setq syntax-code (syntax-class syntax))
    (cond
     ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
      (context-coloring-elisp-colorize-parenthesized-sexp))
     ((or (= syntax-code context-coloring-WORD-CODE)
          (= syntax-code context-coloring-SYMBOL-CODE))
      (context-coloring-elisp-colorize-symbol))
     ((= syntax-code context-coloring-EXPRESSION-PREFIX-CODE)
      (context-coloring-elisp-colorize-expression-prefix))
     (t
      (forward-char)))))

(defun context-coloring-elisp-colorize-region (start end)
  (let (syntax
        syntax-code)
    (goto-char start)
    (while (> end (progn (skip-syntax-forward "^()w_'" end)
                         (point)))
      (setq syntax (syntax-after (point)))
      (setq syntax-code (syntax-class syntax))
      (cond
       ((or (= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
            (= syntax-code context-coloring-WORD-CODE)
            (= syntax-code context-coloring-SYMBOL-CODE)
            (= syntax-code context-coloring-EXPRESSION-PREFIX-CODE))
        (context-coloring-elisp-colorize-sexp))
       (t
        (forward-char))))))

(defun context-coloring-elisp-colorize-changed-region (start end)
  (with-silent-modifications
    (save-excursion
      (let ((start (progn (goto-char start)
                          (beginning-of-defun)
                          (point)))
            (end (progn (goto-char end)
                        (end-of-defun)
                        (point))))
        (setq context-coloring-elisp-scope-stack '())
        (context-coloring-elisp-colorize-region start end)))))

(defun context-coloring-elisp-colorize-buffer ()
  (interactive)
  (with-silent-modifications
    (save-excursion
      (setq context-coloring-elisp-scope-stack '())
      (context-coloring-elisp-colorize-region (point-min) (point-max)))))

(defalias 'ccecb 'context-coloring-elisp-colorize-buffer)

;; TODO: Add cases for special forms like `cond'.
;; TODO: Backticks only go one level deep.
;; TODO: Refactor this function into smaller, focused ones so we can parse
;; recursively and easily.
(defun context-coloring-emacs-lisp-colorize ()
  "Color the current buffer by parsing emacs lisp sexps."
  (with-silent-modifications
    (save-excursion
      ;; TODO: Can probably make this lazy to the nearest defun.
      (goto-char (point-min))
      (let* ((inhibit-point-motion-hooks t)
             (end (point-max))
             (iteration-count 0)
             (last-fontified-position (point))
             beginning-of-current-defun
             end-of-current-defun
             (last-ppss-pos (point))
             (ppss (syntax-ppss))
             ppss-depth
             ;; -1 never matches a depth.  This is a minor optimization.
             (scope-stack `(,(context-coloring-make-scope -1 0)))
             (backtick-stack '())
             (let-varlist-stack '())
             (let-var-stack '())
             popped-vars
             one-word-found-p
             in-defun-p
             in-lambda-p
             in-let-p
             in-let*-p
             defun-arglist
             defun-arg
             let-varlist
             let-varlist-type
             variable
             variable-end
             variable-string
             variable-scope-level
             token-pos
             token-syntax
             token-syntax-code
             token-char
             child-0-pos
             child-0-end
             child-0-syntax
             child-0-syntax-code
             child-0-string
             child-1-pos
             child-1-end
             child-1-syntax
             child-1-syntax-code
             child-2-end)
        (while (> end (progn (skip-syntax-forward "^()w_'" end)
                             (point)))
          ;; Sparingly-executed tasks.
          (setq iteration-count (1+ iteration-count))
          (when (zerop (% iteration-count
                          context-coloring-emacs-lisp-iterations-per-pause))
            ;; Fontify until the end of the current defun because doing it in
            ;; chunks based soley on point could result in partial
            ;; re-fontifications over the contents of scopes.
            (save-excursion
              (end-of-defun)
              (setq end-of-current-defun (point))
              (beginning-of-defun)
              (setq beginning-of-current-defun (point)))

            ;; Fontify in chunks.
            (context-coloring-maybe-colorize-comments-and-strings
             last-fontified-position
             (cond
              ;; We weren't actually in a defun, so don't color the next one, as
              ;; that could result in `font-lock' properties being added to it.
              ((> beginning-of-current-defun (point))
               (point))
              (t
               end-of-current-defun)))
            (setq last-fontified-position (point))
            (when (and context-coloring-parse-interruptable-p
                       (input-pending-p))
              (throw 'interrupted t)))

          (setq token-pos (point))
          (setq token-syntax (syntax-after token-pos))
          (setq token-syntax-code (logand #xFFFF (car token-syntax)))
          (setq token-char (char-after))
          (setq ppss (parse-partial-sexp last-ppss-pos token-pos nil nil ppss))
          (setq last-ppss-pos token-pos)
          (cond

           ;; Resolve an invalid state.
           ((cond
             ;; Inside string?
             ((nth 3 ppss)
              (skip-syntax-forward "^\"" end)
              (forward-char)
              t)
             ;; Inside comment?
             ((nth 4 ppss)
              (skip-syntax-forward "^>" end)
              t)))

           ;; Need to check early in case there's a comma.
           ((context-coloring-expression-prefix-p token-syntax-code)
            (forward-char)
            (cond
             ;; Skip top-level symbols.
             ((not (or backtick-stack
                       (= token-char context-coloring-BACKTICK-CHAR)))
              (goto-char (context-coloring-forward-sexp-position)))
             ;; Push a backtick state.
             ((or (= token-char context-coloring-BACKTICK-CHAR)
                  (= token-char context-coloring-COMMA-CHAR))
              (setq backtick-stack (cons (context-coloring-make-backtick
                                          (context-coloring-forward-sexp-position)
                                          (= token-char context-coloring-BACKTICK-CHAR))
                                         backtick-stack)))))

           ;; Pop a backtick state.
           ((and backtick-stack
                 (>= (point) (context-coloring-backtick-get-end (car backtick-stack))))
            (setq backtick-stack (cdr backtick-stack)))

           ;; Restricted by an enabled backtick.
           ((and backtick-stack
                 (context-coloring-backtick-enabled-p backtick-stack))
            (forward-char))

           ((context-coloring-open-parenthesis-p token-syntax-code)
            (forward-char)
            ;; Look for function calls.
            (context-coloring-forward-sws)
            (setq child-0-pos (point))
            (setq child-0-syntax (syntax-after child-0-pos))
            (setq child-0-syntax-code (logand #xFFFF (car child-0-syntax)))
            (cond
             ((context-coloring-emacs-lisp-identifier-syntax-p child-0-syntax-code)
              (setq one-word-found-p t)
              (setq child-0-end (scan-sexps child-0-pos 1))
              (setq child-0-string (buffer-substring-no-properties child-0-pos child-0-end))
              (cond
               ;; Parse a var in a `let' varlist.
               ((and
                 let-varlist-stack
                 (context-coloring-at-stack-depth-p
                  let-varlist-stack
                  ;; 1- because we're inside the varlist.
                  (1- (context-coloring-ppss-depth ppss))))
                (context-coloring-let-varlist-add-var
                 (car let-varlist-stack)
                 (intern child-0-string))
                (setq let-var-stack (cons (context-coloring-ppss-depth ppss)
                                          let-var-stack)))
               ((string-match-p context-coloring-emacs-lisp-defun-regexp child-0-string)
                (setq in-defun-p t))
               ((string-match-p context-coloring-emacs-lisp-lambda-regexp child-0-string)
                (setq in-lambda-p t))
               ((string-match-p context-coloring-emacs-lisp-let-regexp child-0-string)
                (setq in-let-p t)
                (setq let-varlist-type 'let))
               ((string-match-p context-coloring-emacs-lisp-let*-regexp child-0-string)
                (setq in-let*-p t)
                (setq let-varlist-type 'let*)))))
            (when (or in-defun-p
                      in-lambda-p
                      in-let-p
                      in-let*-p)
              (setq scope-stack (cons (context-coloring-make-scope
                                       (context-coloring-ppss-depth ppss)
                                       (1+ (context-coloring-scope-get-level
                                            (car scope-stack))))
                                      scope-stack)))
            ;; TODO: Maybe wasteful but doing this conditionally doesn't make
            ;; much of a difference.
            (context-coloring-colorize-region token-pos
                                              (scan-sexps token-pos 1)
                                              (context-coloring-scope-get-level
                                               (car scope-stack)))
            (cond
             ((or in-defun-p
                  in-lambda-p)
              (goto-char child-0-end)
              (when in-defun-p
                ;; Look for a function name.
                (context-coloring-forward-sws)
                (setq child-1-pos (point))
                (setq child-1-syntax (syntax-after child-1-pos))
                (setq child-1-syntax-code (logand #xFFFF (car child-1-syntax)))
                (cond
                 ((context-coloring-emacs-lisp-identifier-syntax-p child-1-syntax-code)
                  (setq child-1-end (scan-sexps child-1-pos 1))
                  ;; Defuns are global, so use level 0.
                  (context-coloring-colorize-region child-1-pos child-1-end 0)
                  (goto-char child-1-end))))
              ;; Look for an arglist.
              (context-coloring-forward-sws)
              (when (context-coloring-at-open-parenthesis-p)
                ;; (Actually it should be `child-1-end' for `lambda'.)
                (setq child-2-end (context-coloring-forward-sexp-position))
                (setq defun-arglist (read (buffer-substring-no-properties
                                           (point)
                                           child-2-end)))
                (while defun-arglist
                  (setq defun-arg (car defun-arglist))
                  (when (and (symbolp defun-arg)
                             (string-match-p
                              context-coloring-emacs-lisp-arglist-arg-regexp
                              (symbol-name defun-arg)))
                    (context-coloring-scope-add-variable
                     (car scope-stack)
                     defun-arg))
                  (setq defun-arglist (cdr defun-arglist)))
                (goto-char child-2-end))
              ;; Cleanup.
              (setq in-defun-p nil)
              (setq in-lambda-p nil))
             ((or in-let-p
                  in-let*-p)
              (goto-char child-0-end)
              ;; Look for a varlist.
              (context-coloring-forward-sws)
              (setq child-1-pos (point))
              (setq child-1-syntax (syntax-after child-1-pos))
              (setq child-1-syntax-code (logand #xFFFF (car child-1-syntax)))
              (when (context-coloring-open-parenthesis-p child-1-syntax-code)
                ;; Begin parsing the varlist.
                (forward-char)
                (setq let-varlist-stack (cons (context-coloring-make-let-varlist
                                               ;; 1+ because we parsed it at a
                                               ;; higher depth.
                                               (1+ (context-coloring-ppss-depth ppss))
                                               let-varlist-type)
                                              let-varlist-stack)))
              ;; Cleanup.
              (setq in-let-p nil)
              (setq in-let*-p nil))
             (t
              (goto-char (cond
                          ;; If there was a word, continue parsing after it.
                          (one-word-found-p
                           (1+ child-0-end))
                          (t
                           (1+ token-pos))))))
            ;; Cleanup.
            (setq one-word-found-p nil))

           ((context-coloring-emacs-lisp-identifier-syntax-p token-syntax-code)
            (setq variable-end (context-coloring-forward-sexp-position))
            (setq variable-string (buffer-substring-no-properties
                                   token-pos
                                   variable-end))
            (cond
             ;; Ignore constants such as numbers, keywords, t, nil.  These can't
             ;; be rebound, so they should be treated like syntax.
             ((string-match-p context-coloring-ignored-word-regexp variable-string))
             ((keywordp (read variable-string)))
             (t
              (setq variable (intern variable-string))
              (cond
               ;; Parse a `let' varlist's uninitialized var.
               ((and
                 let-varlist-stack
                 (context-coloring-at-stack-depth-p
                  let-varlist-stack
                  ;; 1- because we're inside the varlist.
                  (1- (context-coloring-ppss-depth ppss))))
                (setq let-varlist (car let-varlist-stack))
                (setq let-varlist-type (context-coloring-let-varlist-get-type let-varlist))
                (cond
                 ;; Defer `let' binding until the end of the varlist.
                 ((eq let-varlist-type 'let)
                  (context-coloring-let-varlist-add-var let-varlist variable))
                 ;; Bind a `let*' right away.
                 ((eq let-varlist-type 'let*)
                  (context-coloring-scope-add-variable (car scope-stack) variable))))
               (t
                (setq variable-scope-level
                      (context-coloring-get-variable-level scope-stack variable))
                (when (/= variable-scope-level (context-coloring-scope-get-level
                                                (car scope-stack)))
                  (context-coloring-colorize-region
                   token-pos
                   variable-end
                   variable-scope-level))))))
            (goto-char variable-end))

           ((context-coloring-close-parenthesis-p token-syntax-code)
            (forward-char)
            (setq ppss (parse-partial-sexp last-ppss-pos (point) nil nil ppss))
            (setq last-ppss-pos (point))
            (setq ppss-depth (context-coloring-ppss-depth ppss))
            ;; TODO: Order might matter here but I'm not certain.
            (when (context-coloring-at-stack-depth-p scope-stack ppss-depth)
              (setq scope-stack (cdr scope-stack)))
            (when (and
                   let-var-stack
                   (= (car let-var-stack) ppss-depth))
              (setq let-var-stack (cdr let-var-stack))
              (when (eq (context-coloring-let-varlist-get-type (car let-varlist-stack))
                        'let*)
                (setq popped-vars (context-coloring-let-varlist-pop-vars
                                   (car let-varlist-stack)))))
            (when (and
                   let-varlist-stack
                   (context-coloring-at-stack-depth-p let-varlist-stack ppss-depth))
              (setq popped-vars (context-coloring-let-varlist-pop-vars
                                 (car let-varlist-stack)))
              (setq let-varlist-stack (cdr let-varlist-stack)))
            (while popped-vars
              (context-coloring-scope-add-variable (car scope-stack) (car popped-vars))
              (setq popped-vars (cdr popped-vars))))

           ))
        ;; Fontify the last stretch.
        (context-coloring-maybe-colorize-comments-and-strings
         last-fontified-position
         (point))))))


;;; Shell command scopification / colorization

(defun context-coloring-apply-tokens (tokens)
  "Process a vector of TOKENS to apply context-based coloring to
the current buffer.  Tokens are 3 integers: start, end, level.
The vector is flat, with a new token occurring after every 3rd
element."
  (with-silent-modifications
    (let ((i 0)
          (len (length tokens)))
      (while (< i len)
        (context-coloring-colorize-region
         (elt tokens i)
         (elt tokens (+ i 1))
         (elt tokens (+ i 2)))
        (setq i (+ i 3))))
    (context-coloring-maybe-colorize-comments-and-strings)))

(defun context-coloring-parse-array (array)
  "Parse ARRAY as a flat JSON array of numbers."
  (let ((braceless (substring (context-coloring-trim array) 1 -1)))
    (cond
     ((> (length braceless) 0)
      (vconcat
       (mapcar 'string-to-number (split-string braceless ","))))
     (t
      (vector)))))

(defvar-local context-coloring-scopifier-process nil
  "The single scopifier process that can be running.")

(defun context-coloring-kill-scopifier ()
  "Kill the currently-running scopifier process."
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-scopify-shell-command (command callback)
  "Invoke a scopifier via COMMAND, read its response
asynchronously and invoke CALLBACK with its output."

  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-kill-scopifier)

  ;; Start the process.
  (setq context-coloring-scopifier-process
        (start-process-shell-command "scopifier" nil command))

  (let ((output ""))

    ;; The process may produce output in multiple chunks.  This filter
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
         (funcall callback output))))))

(defun context-coloring-send-buffer-to-scopifier ()
  "Give the scopifier process its input so it can begin
scopifying."
  (process-send-region
   context-coloring-scopifier-process
   (point-min) (point-max))
  (process-send-eof
   context-coloring-scopifier-process))

(defun context-coloring-scopify-and-colorize (command &optional callback)
  "Invoke a scopifier via COMMAND with the current buffer's contents,
read the scopifier's response asynchronously and apply a parsed
list of tokens to `context-coloring-apply-tokens'.

Invoke CALLBACK when complete."
  (let ((buffer (current-buffer)))
    (context-coloring-scopify-shell-command
     command
     (lambda (output)
       (let ((tokens (context-coloring-parse-array output)))
         (with-current-buffer buffer
           (context-coloring-apply-tokens tokens))
         (setq context-coloring-scopifier-process nil)
         (when callback (funcall callback))))))
  (context-coloring-send-buffer-to-scopifier))


;;; Dispatch

(defvar context-coloring-dispatch-hash-table (make-hash-table :test 'eq)
  "Map dispatch strategy names to their corresponding property
  lists, which contain details about the strategies.")

(defvar context-coloring-mode-hash-table (make-hash-table :test 'eq)
  "Map major mode names to dispatch property lists.")

(defun context-coloring-get-dispatch-for-mode (mode)
  "Return the dispatch for MODE (or a derivative mode)."
  (let ((parent mode)
        dispatch)
    (while (and parent
                (not (setq dispatch (gethash parent context-coloring-mode-hash-table)))
                (setq parent (get parent 'derived-mode-parent))))
    dispatch))

(defun context-coloring-define-dispatch (symbol &rest properties)
  "Define a new dispatch named SYMBOL with PROPERTIES.

A \"dispatch\" is a property list describing a strategy for
coloring a buffer.  There are three possible strategies: Parse
and color in a single function (`:colorizer'), parse in a
function that returns scope data (`:scopifier'), or parse with a
shell command that returns scope data (`:command').  In the
latter two cases, the scope data will be used to automatically
color the buffer.

PROPERTIES must include `:modes' and one of `:colorizer',
`:scopifier' or `:command'.

`:modes' - List of major modes this dispatch is valid for.

`:colorizer' - Symbol referring to a function that parses and
colors the buffer.

`:scopifier' - Symbol referring to a function that parses the
buffer a returns a flat vector of start, end and level data.

`:executable' - Optional name of an executable required by
`:command'.

`:command' - Shell command to execute with the current buffer
sent via stdin, and with a flat JSON array of start, end and
level data returned via stdout.

`:version' - Minimum required version that should be printed when
executing `:command' with a \"--version\" flag.  The version
should be numeric, e.g. \"2\", \"19700101\", \"1.2.3\",
\"v1.2.3\" etc.

`:setup' - Arbitrary code to set up this dispatch when
`context-coloring-mode' is enabled.

`:teardown' - Arbitrary code to tear down this dispatch when
`context-coloring-mode' is disabled."
  (let ((modes (plist-get properties :modes))
        (colorizer (plist-get properties :colorizer))
        (scopifier (plist-get properties :scopifier))
        (command (plist-get properties :command)))
    (when (null modes)
      (error "No mode defined for dispatch"))
    (when (not (or colorizer
                   scopifier
                   command))
      (error "No colorizer, scopifier or command defined for dispatch"))
    (puthash symbol properties context-coloring-dispatch-hash-table)
    (dolist (mode modes)
      (puthash mode properties context-coloring-mode-hash-table))))


;;; Colorization

(defvar context-coloring-colorize-hook nil
  "Hooks to run after coloring a buffer.")

(defun context-coloring-colorize (&optional callback)
  "Color the current buffer by function context.

Invoke CALLBACK when complete; see `context-coloring-dispatch'."
  (interactive)
  (context-coloring-dispatch
   (lambda ()
     (when callback (funcall callback))
     (run-hooks 'context-coloring-colorize-hook))))

(defvar-local context-coloring-changed nil
  "Indication that the buffer has changed recently, which implies
that it should be colored again by
`context-coloring-colorize-idle-timer' if that timer is being
used.")

(defun context-coloring-change-function (_start _end _length)
  "Register a change so that a buffer can be colorized soon."
  ;; Tokenization is obsolete if there was a change.
  (context-coloring-kill-scopifier)
  (setq context-coloring-changed t))

(defun context-coloring-maybe-colorize (buffer)
  "Colorize the current buffer if it has changed."
  (when (and (eq buffer (current-buffer))
             context-coloring-changed)
    (setq context-coloring-changed nil)
    (context-coloring-colorize)))


;;; Versioning

(defun context-coloring-parse-version (string)
  "Extract segments of a version STRING into a list.  \"v1.0.0\"
produces (1 0 0), \"19700101\" produces (19700101), etc."
  (let (version)
    (while (string-match "[0-9]+" string)
      (setq version (append version
                            (list (string-to-number (match-string 0 string)))))
      (setq string (substring string (match-end 0))))
    version))

(defun context-coloring-check-version (expected actual)
  "Check that version EXPECTED is less than or equal to ACTUAL."
  (let ((expected (context-coloring-parse-version expected))
        (actual (context-coloring-parse-version actual))
        (continue t)
        (acceptable t))
    (while (and continue expected)
      (let ((an-expected (car expected))
            (an-actual (car actual)))
        (cond
         ((> an-actual an-expected)
          (setq acceptable t)
          (setq continue nil))
         ((< an-actual an-expected)
          (setq acceptable nil)
          (setq continue nil))))
      (setq expected (cdr expected))
      (setq actual (cdr actual)))
    acceptable))

(defvar context-coloring-check-scopifier-version-hook nil
  "Hooks to run after checking the scopifier version.")

(defun context-coloring-check-scopifier-version (&optional callback)
  "Asynchronously invoke CALLBACK with a predicate indicating
whether the current scopifier version satisfies the minimum
version number required for the current major mode."
  (let ((dispatch (context-coloring-get-dispatch-for-mode major-mode)))
    (when dispatch
      (let ((version (plist-get dispatch :version))
            (command (plist-get dispatch :command)))
        (context-coloring-scopify-shell-command
         (context-coloring-join (list command "--version") " ")
         (lambda (output)
           (if (context-coloring-check-version version output)
               (progn
                 (when callback (funcall callback t)))
             (when callback (funcall callback nil)))
           (run-hooks 'context-coloring-check-scopifier-version-hook)))))))


;;; Themes

(defvar context-coloring-theme-hash-table (make-hash-table :test 'eq)
  "Map theme names to theme properties.")

(defun context-coloring-theme-p (theme)
  "Return t if THEME is defined, nil otherwise."
  (and (gethash theme context-coloring-theme-hash-table)))

(defconst context-coloring-level-face-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face"
  "Extract a level from a face.")

(defvar context-coloring-originally-set-theme-hash-table
  (make-hash-table :test 'eq)
  "Cache custom themes who originally set their own
  `context-coloring-level-N-face' faces.")

(defun context-coloring-theme-originally-set-p (theme)
  "Return t if there is a `context-coloring-level-N-face'
originally set for THEME, nil otherwise."
  (let (originally-set)
    (cond
     ;; `setq' might return a non-nil value for the sake of this `cond'.
     ((setq
       originally-set
       (gethash
        theme
        context-coloring-originally-set-theme-hash-table))
      (eq originally-set 'yes))
     (t
      (let* ((settings (get theme 'theme-settings))
             (tail settings)
             found)
        (while (and tail (not found))
          (and (eq (nth 0 (car tail)) 'theme-face)
               (string-match
                context-coloring-level-face-regexp
                (symbol-name (nth 1 (car tail))))
               (setq found t))
          (setq tail (cdr tail)))
        found)))))

(defun context-coloring-cache-originally-set (theme originally-set)
  "Remember if THEME had colors originally set for it.  If
ORIGINALLY-SET is non-nil, it did, otherwise it didn't."
  ;; Caching whether a theme was originally set is kind of dirty, but we have to
  ;; do it to remember the past state of the theme.  There are probably some
  ;; edge cases where caching will be an issue, but they are probably rare.
  (puthash
   theme
   (if originally-set 'yes 'no)
   context-coloring-originally-set-theme-hash-table))

(defun context-coloring-warn-theme-originally-set (theme)
  "Warn the user that the colors for THEME are already originally
set."
  (warn "Context coloring colors for theme `%s' are already defined" theme))

(defun context-coloring-theme-highest-level (theme)
  "Return the highest level N of a face like
`context-coloring-level-N-face' set for THEME, or `-1' if there
is none."
  (let* ((settings (get theme 'theme-settings))
         (tail settings)
         face-string
         number
         (found -1))
    (while tail
      (and (eq (nth 0 (car tail)) 'theme-face)
           (setq face-string (symbol-name (nth 1 (car tail))))
           (string-match
            context-coloring-level-face-regexp
            face-string)
           (setq number (string-to-number
                         (substring face-string
                                    (match-beginning 1)
                                    (match-end 1))))
           (> number found)
           (setq found number))
      (setq tail (cdr tail)))
    found))

(defun context-coloring-apply-theme (theme)
  "Apply THEME's properties to its respective custom theme,
which must already exist and which *should* already be enabled."
  (let* ((properties (gethash theme context-coloring-theme-hash-table))
         (colors (plist-get properties :colors))
         (level -1))
    ;; Only clobber when we have to.
    (when (custom-theme-enabled-p theme)
      (setq context-coloring-maximum-face (- (length colors) 1)))
    (apply
     'custom-theme-set-faces
     theme
     (mapcar
      (lambda (color)
        (setq level (+ level 1))
        `(,(context-coloring-level-face level) ((t (:foreground ,color)))))
      colors))))

(defun context-coloring-define-theme (theme &rest properties)
  "Define a context theme named THEME for coloring scope levels.

PROPERTIES is a property list specifiying the following details:

`:aliases': List of symbols of other custom themes that these
colors are applicable to.

`:colors': List of colors that this context theme uses.

`:override': If non-nil, this context theme is intentionally
overriding colors set by a custom theme.  Don't set this non-nil
unless there is a custom theme you want to use which sets
`context-coloring-level-N-face' faces that you want to replace.

`:recede': If non-nil, this context theme should not apply its
colors if a custom theme already sets
`context-coloring-level-N-face' faces.  This option is
optimistic; set this non-nil if you would rather confer the duty
of picking colors to a custom theme author (if / when he ever
gets around to it).

By default, context themes will always override custom themes,
even if those custom themes set `context-coloring-level-N-face'
faces.  If a context theme does override a custom theme, a
warning will be raised, at which point you may want to enable the
`:override' option, or just delete your context theme and opt to
use your custom theme's author's colors instead.

Context themes only work for the custom theme with the highest
precedence, i.e. the car of `custom-enabled-themes'."
  (let ((aliases (plist-get properties :aliases))
        (override (plist-get properties :override))
        (recede (plist-get properties :recede)))
    (dolist (name (append `(,theme) aliases))
      (puthash name properties context-coloring-theme-hash-table)
      (when (custom-theme-p name)
        (let ((originally-set (context-coloring-theme-originally-set-p name)))
          (context-coloring-cache-originally-set name originally-set)
          ;; In the particular case when you innocently define colors that a
          ;; custom theme originally set, warn.  Arguably this only has to be
          ;; done at enable time, but it is probably more useful to do it at
          ;; definition time for prompter feedback.
          (when (and originally-set
                     (not recede)
                     (not override))
            (context-coloring-warn-theme-originally-set name))
          ;; Set (or overwrite) colors.
          (when (not (and originally-set
                          recede))
            (context-coloring-apply-theme name)))))))

(defun context-coloring-enable-theme (theme)
  "Apply THEME if its colors are not already set, else just set
`context-coloring-maximum-face' to the correct value for THEME."
  (let* ((properties (gethash theme context-coloring-theme-hash-table))
         (recede (plist-get properties :recede))
         (override (plist-get properties :override)))
    (cond
     (recede
      (let ((highest-level (context-coloring-theme-highest-level theme)))
        (cond
         ;; This can be true whether originally set by a custom theme or by a
         ;; context theme.
         ((> highest-level -1)
          (setq context-coloring-maximum-face highest-level))
         ;; It is possible that the corresponding custom theme did not exist at
         ;; the time of defining this context theme, and in that case the above
         ;; condition proves the custom theme did not originally set any faces,
         ;; so we have license to apply the context theme for the first time
         ;; here.
         (t
          (context-coloring-apply-theme theme)))))
     (t
      (let ((originally-set (context-coloring-theme-originally-set-p theme)))
        ;; Cache now in case the context theme was defined after the custom
        ;; theme.
        (context-coloring-cache-originally-set theme originally-set)
        (when (and originally-set
                   (not override))
          (context-coloring-warn-theme-originally-set theme))
        (context-coloring-apply-theme theme))))))

(defadvice enable-theme (after context-coloring-enable-theme (theme) activate)
  "Enable colors for context themes just-in-time."
  (when (and (not (eq theme 'user)) ; Called internally by `enable-theme'.
             (custom-theme-p theme) ; Guard against non-existent themes.
             (context-coloring-theme-p theme))
    (when (= (length custom-enabled-themes) 1)
      ;; Cache because we can't reliably figure it out in reverse.
      (setq context-coloring-original-maximum-face
            context-coloring-maximum-face))
    (context-coloring-enable-theme theme)))

(defadvice disable-theme (after context-coloring-disable-theme (theme) activate)
  "Update `context-coloring-maximum-face'."
  (when (custom-theme-p theme) ; Guard against non-existent themes.
    (let ((enabled-theme (car custom-enabled-themes)))
      (if (context-coloring-theme-p enabled-theme)
          (progn
            (context-coloring-enable-theme enabled-theme))
        ;; Assume we are back to no theme; act as if nothing ever happened.
        ;; This is still prone to intervention, but rather extraordinarily.
        (setq context-coloring-maximum-face
              context-coloring-original-maximum-face)))))

(context-coloring-define-theme
 'ample
 :recede t
 :colors '("#bdbdb3"
           "#baba36"
           "#6aaf50"
           "#5180b3"
           "#ab75c3"
           "#cd7542"
           "#df9522"
           "#454545"))

(context-coloring-define-theme
 'anti-zenburn
 :recede t
 :colors '("#232333"
           "#6c1f1c"
           "#401440"
           "#0f2050"
           "#205070"
           "#336c6c"
           "#23733c"
           "#6b400c"
           "#603a60"
           "#2f4070"
           "#235c5c"))

(context-coloring-define-theme
 'grandshell
 :recede t
 :colors '("#bebebe"
           "#5af2ee"
           "#b2baf6"
           "#f09fff"
           "#efc334"
           "#f6df92"
           "#acfb5a"
           "#888888"))

(context-coloring-define-theme
 'leuven
 :recede t
 :colors '("#333333"
           "#0000ff"
           "#6434a3"
           "#ba36a5"
           "#d0372d"
           "#036a07"
           "#006699"
           "#006fe0"
           "#808080"))

(context-coloring-define-theme
 'monokai
 :recede t
 :colors '("#f8f8f2"
           "#66d9ef"
           "#a1efe4"
           "#a6e22e"
           "#e6db74"
           "#fd971f"
           "#f92672"
           "#fd5ff0"
           "#ae81ff"))

(context-coloring-define-theme
 'solarized
 :recede t
 :aliases '(solarized-light
            solarized-dark
            sanityinc-solarized-light
            sanityinc-solarized-dark)
 :colors '("#839496"
           "#268bd2"
           "#2aa198"
           "#859900"
           "#b58900"
           "#cb4b16"
           "#dc322f"
           "#d33682"
           "#6c71c4"
           "#69b7f0"
           "#69cabf"
           "#b4c342"
           "#deb542"
           "#f2804f"
           "#ff6e64"
           "#f771ac"
           "#9ea0e5"))

(context-coloring-define-theme
 'spacegray
 :recede t
 :colors '("#ffffff"
           "#89aaeb"
           "#c189eb"
           "#bf616a"
           "#dca432"
           "#ebcb8b"
           "#b4eb89"
           "#89ebca"))

(context-coloring-define-theme
 'tango
 :recede t
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
 :recede t
 :colors '("#dcdccc"
           "#93e0e3"
           "#bfebbf"
           "#f0dfaf"
           "#dfaf8f"
           "#cc9393"
           "#dc8cc3"
           "#94bff3"
           "#9fc59f"
           "#d0bf8f"
           "#dca3a3"))


;;; Change detection

(defvar-local context-coloring-colorize-idle-timer nil
  "The currently-running idle timer.")

(defcustom context-coloring-delay 0.25
  "Delay between a buffer update and colorization.

Increase this if your machine is high-performing.  Decrease it if
it ain't.

Supported modes: `js-mode', `js3-mode', `emacs-lisp-mode'"
  :group 'context-coloring)

(defun context-coloring-setup-idle-change-detection ()
  "Setup idle change detection."
  (add-hook
   'after-change-functions 'context-coloring-change-function nil t)
  (add-hook
   'kill-buffer-hook 'context-coloring-teardown-idle-change-detection nil t)
  (setq context-coloring-colorize-idle-timer
        (run-with-idle-timer
         context-coloring-delay
         t
         'context-coloring-maybe-colorize
         (current-buffer))))

(defun context-coloring-teardown-idle-change-detection ()
  "Teardown idle change detection."
  (context-coloring-kill-scopifier)
  (when context-coloring-colorize-idle-timer
    (cancel-timer context-coloring-colorize-idle-timer))
  (remove-hook
   'kill-buffer-hook 'context-coloring-teardown-idle-change-detection t)
  (remove-hook
   'after-change-functions 'context-coloring-change-function t))


;;; Built-in dispatches

(context-coloring-define-dispatch
 'javascript-node
 :modes '(js-mode js3-mode)
 :executable "scopifier"
 :command "scopifier"
 :version "v1.1.1")

(context-coloring-define-dispatch
 'javascript-js2
 :modes '(js2-mode)
 :colorizer 'context-coloring-js2-colorize
 :setup
 (lambda ()
   (add-hook 'js2-post-parse-callbacks 'context-coloring-colorize nil t))
 :teardown
 (lambda ()
   (remove-hook 'js2-post-parse-callbacks 'context-coloring-colorize t)))

(context-coloring-define-dispatch
 'emacs-lisp
 :modes '(emacs-lisp-mode)
 :colorizer 'context-coloring-elisp-colorize-buffer
 :setup 'context-coloring-setup-idle-change-detection
 :teardown 'context-coloring-teardown-idle-change-detection)

(defun context-coloring-dispatch (&optional callback)
  "Determine the optimal track for scopification / coloring of
the current buffer, then execute it.

Invoke CALLBACK when complete.  It is invoked synchronously for
elisp tracks, and asynchronously for shell command tracks."
  (let* ((dispatch (context-coloring-get-dispatch-for-mode major-mode))
         (colorizer (plist-get dispatch :colorizer))
         (scopifier (plist-get dispatch :scopifier))
         (command (plist-get dispatch :command))
         interrupted-p)
    (cond
     ((or colorizer scopifier)
      (setq interrupted-p
            (catch 'interrupted
              (cond
               (colorizer
                (funcall colorizer))
               (scopifier
                (context-coloring-apply-tokens (funcall scopifier))))))
      (cond
       (interrupted-p
        (setq context-coloring-changed t))
       (t
        (when callback (funcall callback)))))
     (command
      (context-coloring-scopify-and-colorize command callback)))))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        (let ((dispatch (context-coloring-get-dispatch-for-mode major-mode)))
          (when dispatch
            (let ((command (plist-get dispatch :command))
                  (teardown (plist-get dispatch :teardown)))
              (when command
                (context-coloring-teardown-idle-change-detection))
              (when teardown
                (funcall teardown)))))
        (font-lock-mode)
        (jit-lock-mode t))

    ;; Font lock is incompatible with this mode; the converse is also true.
    (font-lock-mode 0)
    (jit-lock-mode nil)

    ;; ...but we do use font-lock functions here.
    (font-lock-set-defaults)

    ;; Safely change the valye of this function as necessary.
    (make-local-variable 'font-lock-syntactic-face-function)

    (let ((dispatch (context-coloring-get-dispatch-for-mode major-mode)))
      (if dispatch
          (progn
            (let ((command (plist-get dispatch :command))
                  (version (plist-get dispatch :version))
                  (executable (plist-get dispatch :executable))
                  (setup (plist-get dispatch :setup))
                  (colorize-initially-p t))
              (when command
                ;; Shell commands recolor on change, idly.
                (cond
                 ((and executable
                       (null (executable-find executable)))
                  (message "Executable \"%s\" not found" executable)
                  (setq colorize-initially-p nil))
                 (version
                  (context-coloring-check-scopifier-version
                   (lambda (sufficient-p)
                     (if sufficient-p
                         (progn
                           (context-coloring-setup-idle-change-detection)
                           (context-coloring-colorize))
                       (message "Update to the minimum version of \"%s\" (%s)"
                                executable version))))
                  (setq colorize-initially-p nil))
                 (t
                  (context-coloring-setup-idle-change-detection))))
              (when setup
                (funcall setup))
              ;; Colorize once initially.
              (when colorize-initially-p
                (let ((context-coloring-parse-interruptable-p nil))
                  (context-coloring-colorize)))))
        (when (null dispatch)
          (message "Context coloring is not available for this major mode"))))))

(provide 'context-coloring)

;;; context-coloring.el ends here
