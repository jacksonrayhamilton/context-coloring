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
  (mapconcat #'identity strings delimiter))

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


;;; Change detection

(defvar-local context-coloring-changed-p nil
  "Indication that the buffer has changed recently, which implies
that it should be colored again by
`context-coloring-colorize-idle-timer' if that timer is being
used.")

(defvar-local context-coloring-changed-start nil
  "Beginning of last text that changed.")

(defvar-local context-coloring-changed-end nil
  "End of last text that changed.")

(defvar-local context-coloring-changed-length nil
  "Length of last text that changed.")

(defun context-coloring-change-function (start end length)
  "Register a change so that a buffer can be colorized soon.

START, END and LENGTH are recorded for later use."
  ;; Tokenization is obsolete if there was a change.
  (context-coloring-cancel-scopification)
  (setq context-coloring-changed-start start)
  (setq context-coloring-changed-end end)
  (setq context-coloring-changed-length length)
  (setq context-coloring-changed-p t))

(defun context-coloring-maybe-colorize (buffer)
  "Colorize the current buffer if it is BUFFER and has changed."
  (when (and (eq buffer (current-buffer))
             context-coloring-changed-p)
    (context-coloring-colorize)
    (setq context-coloring-changed-p nil)
    (setq context-coloring-changed-start nil)
    (setq context-coloring-changed-end nil)
    (setq context-coloring-changed-length nil)))

(defvar-local context-coloring-colorize-idle-timer nil
  "The currently-running idle timer.")

(defcustom context-coloring-default-delay 0.25
  "Default (sometimes overridden) delay between a buffer update
and colorization.

Increase this if your machine is high-performing.  Decrease it if
it ain't.

Supported modes: `js-mode', `js3-mode'"
  :group 'context-coloring)

(make-obsolete-variable
 'context-coloring-delay
 'context-coloring-default-delay
 "6.4.0")

(defun context-coloring-setup-idle-change-detection ()
  "Setup idle change detection."
  (let ((dispatch (context-coloring-get-dispatch-for-mode major-mode)))
    (add-hook
     'after-change-functions #'context-coloring-change-function nil t)
    (add-hook
     'kill-buffer-hook #'context-coloring-teardown-idle-change-detection nil t)
    (setq context-coloring-colorize-idle-timer
          (run-with-idle-timer
           (or (plist-get dispatch :delay) context-coloring-default-delay)
           t
           #'context-coloring-maybe-colorize
           (current-buffer)))))

(defun context-coloring-teardown-idle-change-detection ()
  "Teardown idle change detection."
  (context-coloring-cancel-scopification)
  (when context-coloring-colorize-idle-timer
    (cancel-timer context-coloring-colorize-idle-timer))
  (remove-hook
   'kill-buffer-hook #'context-coloring-teardown-idle-change-detection t)
  (remove-hook
   'after-change-functions #'context-coloring-change-function t))


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

(defsubst context-coloring-colorize-comments-and-strings (&optional min max)
  "Color the current buffer's comments or strings if
`context-coloring-syntactic-comments' or
`context-coloring-syntactic-strings' are non-nil.  MIN defaults
to the beginning of the buffer and MAX defaults to the end."
  (when (or context-coloring-syntactic-comments
            context-coloring-syntactic-strings)
    (let ((min (or min (point-min)))
          (max (or max (point-max)))
          (font-lock-syntactic-face-function
           (cond
            ((and context-coloring-syntactic-comments
                  (not context-coloring-syntactic-strings))
             #'context-coloring-font-lock-syntactic-comment-function)
            ((and context-coloring-syntactic-strings
                  (not context-coloring-syntactic-comments))
             #'context-coloring-font-lock-syntactic-string-function)
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
  (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test #'eq))
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
    (context-coloring-colorize-comments-and-strings)))


;;; Emacs Lisp colorization

(defsubst context-coloring-forward-sws ()
  "Move forward through whitespace and comments."
  (while (forward-comment 1)))

(defsubst context-coloring-elisp-forward-sws ()
  "Move forward through whitespace and comments, colorizing
comments along the way."
  (let ((start (point)))
    (context-coloring-forward-sws)
    (context-coloring-colorize-comments-and-strings start (point))))

(defsubst context-coloring-elisp-forward-sexp ()
  "Like `forward-sexp', but colorize comments and strings along
the way."
  (let ((start (point)))
    (forward-sexp)
    (context-coloring-elisp-colorize-comments-and-strings-in-region
     start (point))))

(defsubst context-coloring-get-syntax-code ()
  "Get the syntax code at point."
  (syntax-class
   ;; Faster version of `syntax-after':
   (aref (syntax-table) (char-after (point)))))

(defsubst context-coloring-exact-regexp (word)
  "Create a regexp matching exactly WORD."
  (concat "\\`" (regexp-quote word) "\\'"))

(defsubst context-coloring-exact-or-regexp (words)
  "Create a regexp matching any exact word in WORDS."
  (context-coloring-join
   (mapcar #'context-coloring-exact-regexp words) "\\|"))

(defconst context-coloring-elisp-defun-regexp
  (context-coloring-exact-or-regexp
   '("defun" "defun*" "defsubst" "defmacro"
     "cl-defun" "cl-defsubst" "cl-defmacro")))

(defconst context-coloring-elisp-condition-case-regexp
  (context-coloring-exact-or-regexp
   '("condition-case"
     "condition-case-unless-debug")))

(defconst context-coloring-elisp-dolist-regexp
  (context-coloring-exact-or-regexp
   '("dolist" "dotimes")))

(defconst context-coloring-elisp-ignored-word-regexp
  (context-coloring-join (list "\\`[-+]?[0-9]"
                               "\\`[&:].+"
                               (context-coloring-exact-or-regexp
                                '("t" "nil" "." "?")))
                         "\\|")
  "Match words that might be considered symbols but can't be
bound as variables.")

(defconst context-coloring-WORD-CODE 2)
(defconst context-coloring-SYMBOL-CODE 3)
(defconst context-coloring-OPEN-PARENTHESIS-CODE 4)
(defconst context-coloring-CLOSE-PARENTHESIS-CODE 5)
(defconst context-coloring-EXPRESSION-PREFIX-CODE 6)
(defconst context-coloring-STRING-QUOTE-CODE 7)
(defconst context-coloring-ESCAPE-CODE 9)
(defconst context-coloring-COMMENT-START-CODE 11)
(defconst context-coloring-COMMENT-END-CODE 12)

(defconst context-coloring-OCTOTHORPE-CHAR (string-to-char "#"))
(defconst context-coloring-APOSTROPHE-CHAR (string-to-char "'"))
(defconst context-coloring-OPEN-PARENTHESIS-CHAR (string-to-char "("))
(defconst context-coloring-COMMA-CHAR (string-to-char ","))
(defconst context-coloring-AT-CHAR (string-to-char "@"))
(defconst context-coloring-BACKTICK-CHAR (string-to-char "`"))

(defsubst context-coloring-elisp-identifier-p (syntax-code)
  "Check if SYNTAX-CODE is an emacs lisp identifier constituent."
  (or (= syntax-code context-coloring-WORD-CODE)
      (= syntax-code context-coloring-SYMBOL-CODE)))

(defvar context-coloring-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.")

(defconst context-coloring-elisp-sexps-per-pause 1000
  "Pause after this many iterations to check for user input.
If user input is pending, stop the parse.  This makes for a
smoother user experience for large files.")

(defvar context-coloring-elisp-sexp-count 0
  "Current number of sexps leading up to the next pause.")

(defsubst context-coloring-elisp-increment-sexp-count ()
  "Maybe check if the current parse should be interrupted as a
result of pending user input."
  (setq context-coloring-elisp-sexp-count
        (1+ context-coloring-elisp-sexp-count))
  (when (and (zerop (% context-coloring-elisp-sexp-count
                       context-coloring-elisp-sexps-per-pause))
             context-coloring-parse-interruptable-p
             (input-pending-p))
    (throw 'interrupted t)))

(defvar context-coloring-elisp-scope-stack '()
  "List of scopes in the current parse.")

(defsubst context-coloring-elisp-make-scope (level)
  "Make a scope object for LEVEL."
  (list
   :level level
   :variables '()))

(defsubst context-coloring-elisp-scope-get-level (scope)
  "Get the level of SCOPE object."
  (plist-get scope :level))

(defsubst context-coloring-elisp-scope-add-variable (scope variable)
  "Add to SCOPE a VARIABLE."
  (plist-put scope :variables (cons variable (plist-get scope :variables))))

(defsubst context-coloring-elisp-scope-has-variable (scope variable)
  "Check if SCOPE has VARIABLE."
  (member variable (plist-get scope :variables)))

(defsubst context-coloring-elisp-get-variable-level (variable)
  "Search up the scope chain for the first instance of VARIABLE
and return its level, or 0 (global) if it isn't found."
  (let* ((scope-stack context-coloring-elisp-scope-stack)
         scope
         level)
    (while (and scope-stack (not level))
      (setq scope (car scope-stack))
      (cond
       ((context-coloring-elisp-scope-has-variable scope variable)
        (setq level (context-coloring-elisp-scope-get-level scope)))
       (t
        (setq scope-stack (cdr scope-stack)))))
    ;; Assume a global variable.
    (or level 0)))

(defsubst context-coloring-elisp-get-current-scope-level ()
  "Get the nesting level of the current scope."
  (cond
   ((car context-coloring-elisp-scope-stack)
    (context-coloring-elisp-scope-get-level (car context-coloring-elisp-scope-stack)))
   (t
    0)))

(defsubst context-coloring-elisp-push-scope ()
  "Add a new scope to the bottom of the scope chain."
  (push (context-coloring-elisp-make-scope
         (1+ (context-coloring-elisp-get-current-scope-level)))
        context-coloring-elisp-scope-stack))

(defsubst context-coloring-elisp-pop-scope ()
  "Remove the scope on the bottom of the scope chain."
  (pop context-coloring-elisp-scope-stack))

(defsubst context-coloring-elisp-add-variable (variable)
  "Add VARIABLE to the current scope."
  (context-coloring-elisp-scope-add-variable
   (car context-coloring-elisp-scope-stack)
   variable))

(defsubst context-coloring-elisp-parse-bindable (callback)
  "Parse the symbol at point, and if the symbol can be bound,
invoke CALLBACK with it."
  (let* ((arg-string (buffer-substring-no-properties
                      (point)
                      (progn (context-coloring-elisp-forward-sexp)
                             (point)))))
    (when (not (string-match-p
                context-coloring-elisp-ignored-word-regexp
                arg-string))
      (funcall callback arg-string))))

(defun context-coloring-elisp-parse-let-varlist (type)
  "Parse the list of variable initializers at point.  If TYPE is
`let', all the variables are bound after all their initializers
are parsed; if TYPE is `let*', each variable is bound immediately
after its own initializer is parsed."
  (let ((varlist '())
        syntax-code)
    ;; Enter.
    (forward-char)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (forward-char)
        (context-coloring-elisp-forward-sws)
        (setq syntax-code (context-coloring-get-syntax-code))
        (when (context-coloring-elisp-identifier-p syntax-code)
          (context-coloring-elisp-parse-bindable
           (lambda (var)
             (push var varlist)))
          (context-coloring-elisp-forward-sws)
          (setq syntax-code (context-coloring-get-syntax-code))
          (when (/= syntax-code context-coloring-CLOSE-PARENTHESIS-CODE)
            (context-coloring-elisp-colorize-sexp)))
        (context-coloring-elisp-forward-sws)
        ;; Skip past the closing parenthesis.
        (forward-char))
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-parse-bindable
         (lambda (var)
           (push var varlist))))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (when (eq type 'let*)
        (context-coloring-elisp-add-variable (pop varlist)))
      (context-coloring-elisp-forward-sws))
    (when (eq type 'let)
      (while varlist
        (context-coloring-elisp-add-variable (pop varlist))))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-parse-arglist ()
  "Parse the list of function arguments at point."
  (let (syntax-code)
    ;; Enter.
    (forward-char)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-parse-bindable
         (lambda (arg)
           (context-coloring-elisp-add-variable arg))))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (context-coloring-elisp-forward-sws))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-colorize-defun-like (&optional anonymous-p
                                                             let-type)
  "Color the defun-like function at point.  ANONYMOUS-P indicates
the function doesn't name itself (e.g. `lambda', `let').
LET-TYPE can be one of `let' or `let*'."
  (let ((start (point))
        end
        stop
        syntax-code
        defun-name-pos
        defun-name-end)
    (context-coloring-elisp-push-scope)
    ;; Color the whole sexp.
    (forward-sexp)
    (setq end (point))
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    (goto-char start)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    ;; Skip past the "defun".
    (forward-sexp)
    (context-coloring-elisp-forward-sws)
    (setq stop nil)
    (unless anonymous-p
      ;; Check for the defun's name.
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((context-coloring-elisp-identifier-p syntax-code)
        ;; Color the defun's name with the top-level color.
        (setq defun-name-pos (point))
        (forward-sexp)
        (setq defun-name-end (point))
        (context-coloring-colorize-region defun-name-pos defun-name-end 0)
        (context-coloring-elisp-forward-sws))
       (t
        (setq stop t))))
    (cond
     (stop
      ;; Skip it.
      (goto-char start)
      (context-coloring-elisp-forward-sexp))
     (t
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (cond
         (let-type
          (context-coloring-elisp-parse-let-varlist let-type))
         (t
          (context-coloring-elisp-parse-arglist)))
        ;; Colorize the rest of the function.
        (context-coloring-elisp-colorize-region (point) (1- end))
        ;; Exit the defun.
        (forward-char))
       (t
        ;; Skip it.
        (goto-char start)
        (context-coloring-elisp-forward-sexp)))))
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-colorize-defun ()
  "Color the `defun' (or defun-like function) at point."
  (context-coloring-elisp-colorize-defun-like))

(defun context-coloring-elisp-colorize-lambda ()
  "Color the `lambda' at point."
  (context-coloring-elisp-colorize-defun-like t))

(defun context-coloring-elisp-colorize-let ()
  "Color the `let' at point."
  (context-coloring-elisp-colorize-defun-like t 'let))

(defun context-coloring-elisp-colorize-let* ()
  "Color the `let*' at point."
  (context-coloring-elisp-colorize-defun-like t 'let*))

(defun context-coloring-elisp-colorize-cond ()
  "Color the `cond' at point."
  (let (syntax-code)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    ;; Skip past the "cond".
    (forward-sexp)
    (context-coloring-elisp-forward-sws)
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        ;; Colorize inside the parens.
        (let ((start (point)))
          (forward-sexp)
          (context-coloring-elisp-colorize-region
           (1+ start) (1- (point)))
          ;; Exit.
          (forward-char)))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (context-coloring-elisp-forward-sws))
    ;; Exit.
    (forward-char)))

(defun context-coloring-elisp-colorize-condition-case ()
  "Color the `condition-case' at point."
  (let ((start (point))
        end
        syntax-code
        variable
        case-pos
        case-end)
    (context-coloring-elisp-push-scope)
    ;; Color the whole sexp.
    (forward-sexp)
    (setq end (point))
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    (goto-char start)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    ;; Skip past the "condition-case".
    (forward-sexp)
    (context-coloring-elisp-forward-sws)
    (setq syntax-code (context-coloring-get-syntax-code))
    ;; Gracefully ignore missing variables.
    (when (context-coloring-elisp-identifier-p syntax-code)
      (context-coloring-elisp-parse-bindable
       (lambda (parsed-variable)
         (setq variable parsed-variable)))
      (context-coloring-elisp-forward-sws))
    (context-coloring-elisp-colorize-sexp)
    (context-coloring-elisp-forward-sws)
    ;; Parse the handlers with the error variable in scope.
    (when variable
      (context-coloring-elisp-add-variable variable))
    (while (/= (setq syntax-code (context-coloring-get-syntax-code))
               context-coloring-CLOSE-PARENTHESIS-CODE)
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (setq case-pos (point))
        (context-coloring-elisp-forward-sexp)
        (setq case-end (point))
        (goto-char case-pos)
        ;; Enter.
        (forward-char)
        (context-coloring-elisp-forward-sws)
        (setq syntax-code (context-coloring-get-syntax-code))
        (when (/= syntax-code context-coloring-CLOSE-PARENTHESIS-CODE)
          ;; Skip the condition name(s).
          (context-coloring-elisp-forward-sexp)
          ;; Color the remaining portion of the handler.
          (context-coloring-elisp-colorize-region
           (point)
           (1- case-end)))
        ;; Exit.
        (forward-char))
       (t
        ;; Ignore artifacts.
        (context-coloring-elisp-forward-sexp)))
      (context-coloring-elisp-forward-sws))
    ;; Exit.
    (forward-char)
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-colorize-scope (callback)
  "Color the whole scope at point with its one color.  Handle a
header in CALLBACK."
  (let ((start (point))
        (end (progn (forward-sexp)
                    (point))))
    (context-coloring-elisp-push-scope)
    ;; Splash the whole thing in one color.
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    ;; Even if the parse is interrupted, this region should still be colored
    ;; syntactically.
    (context-coloring-elisp-colorize-comments-and-strings-in-region
     start
     end)
    (goto-char start)
    ;; Enter.
    (forward-char)
    (context-coloring-elisp-forward-sws)
    ;; Skip past the function name.
    (forward-sexp)
    (context-coloring-elisp-forward-sws)
    (funcall callback)
    (context-coloring-elisp-colorize-region (point) (1- end))
    ;; Exit.
    (forward-char)
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-colorize-dolist ()
  "Color the `dolist' at point."
  (let (syntax-code
        (index 0))
    (context-coloring-elisp-colorize-scope
     (lambda ()
       (setq syntax-code (context-coloring-get-syntax-code))
       (when (= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
         (forward-char)
         (context-coloring-elisp-forward-sws)
         (while (/= (setq syntax-code (context-coloring-get-syntax-code))
                    context-coloring-CLOSE-PARENTHESIS-CODE)
           (cond
            ((and
              (or (= index 0) (= index 2))
              (context-coloring-elisp-identifier-p syntax-code))
             ;; Add the first or third name to the scope.
             (context-coloring-elisp-parse-bindable
              (lambda (variable)
                (context-coloring-elisp-add-variable variable))))
            (t
             ;; Color artifacts.
             (context-coloring-elisp-colorize-sexp)))
           (context-coloring-elisp-forward-sws)
           (setq index (1+ index)))
         ;; Exit.
         (forward-char))))))

(defun context-coloring-elisp-colorize-parenthesized-sexp ()
  "Color the sexp enclosed by parenthesis at point."
  (context-coloring-elisp-increment-sexp-count)
  (let* ((start (point))
         (end (progn (forward-sexp)
                     (point)))
         (syntax-code (progn (goto-char start)
                             (forward-char)
                             ;; Coloring is unnecessary here, it'll happen
                             ;; presently.
                             (context-coloring-forward-sws)
                             (context-coloring-get-syntax-code))))
    ;; Figure out if the sexp is a special form.
    (cond
     ((when (context-coloring-elisp-identifier-p syntax-code)
        (let ((name-string (buffer-substring-no-properties
                            (point)
                            (progn (forward-sexp)
                                   (point)))))
          (cond
           ((string-match-p context-coloring-elisp-defun-regexp name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-defun)
            t)
           ((string-equal "let" name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-let)
            t)
           ((string-equal "let*" name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-let*)
            t)
           ((string-equal "lambda" name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-lambda)
            t)
           ((string-equal "cond" name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-cond)
            t)
           ((string-match-p context-coloring-elisp-condition-case-regexp name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-condition-case)
            t)
           ((string-match-p context-coloring-elisp-dolist-regexp name-string)
            (goto-char start)
            (context-coloring-elisp-colorize-dolist)
            t)
           (t
            nil)))))
     ;; Not a special form; just colorize the remaining region.
     (t
      (context-coloring-colorize-region
       start
       end
       (context-coloring-elisp-get-current-scope-level))
      (context-coloring-elisp-colorize-region (point) (1- end))
      (forward-char)))))

(defun context-coloring-elisp-colorize-symbol ()
  "Color the symbol at point."
  (context-coloring-elisp-increment-sexp-count)
  (let* ((symbol-pos (point))
         (symbol-end (progn (forward-sexp)
                            (point)))
         (symbol-string (buffer-substring-no-properties
                         symbol-pos
                         symbol-end)))
    (cond
     ((string-match-p context-coloring-elisp-ignored-word-regexp symbol-string))
     (t
      (context-coloring-colorize-region
       symbol-pos
       symbol-end
       (context-coloring-elisp-get-variable-level
        symbol-string))))))

(defun context-coloring-elisp-colorize-expression-prefix ()
  "Color the expression prefix and the following expression at
point.  It could be a quoted or backquoted expression."
  (context-coloring-elisp-increment-sexp-count)
  (let ((char (char-after))
        start
        end)
    (cond
     ((or (= char context-coloring-APOSTROPHE-CHAR)
          (= char context-coloring-OCTOTHORPE-CHAR))
      (context-coloring-elisp-forward-sexp))
     ((= char context-coloring-BACKTICK-CHAR)
      (setq start (point))
      (setq end (progn (forward-sexp)
                       (point)))
      (goto-char start)
      (while (> end (progn (forward-char)
                           (point)))
        (setq char (char-after))
        (when (= char context-coloring-COMMA-CHAR)
          (forward-char)
          (when (= (char-after) context-coloring-AT-CHAR)
            ;; If we don't do this "@" could be interpreted as a symbol.
            (forward-char))
          (context-coloring-elisp-forward-sws)
          (context-coloring-elisp-colorize-sexp)))
      ;; We could probably do this as part of the above loop but it'd be
      ;; repetitive.
      (context-coloring-elisp-colorize-comments-and-strings-in-region
       start end)))))

(defun context-coloring-elisp-colorize-comment ()
  "Color the comment at point."
  (context-coloring-elisp-increment-sexp-count)
  (context-coloring-elisp-forward-sws))

(defun context-coloring-elisp-colorize-string ()
  "Color the string at point."
  (context-coloring-elisp-increment-sexp-count)
  (let ((start (point)))
    (forward-sexp)
    (context-coloring-colorize-comments-and-strings start (point))))

;; Elisp has whitespace, words, symbols, open/close parenthesis, expression
;; prefix, string quote, comment starters/enders and escape syntax classes only.

(defun context-coloring-elisp-colorize-sexp ()
  "Color the sexp at point."
  (let ((syntax-code (context-coloring-get-syntax-code)))
    (cond
     ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
      (context-coloring-elisp-colorize-parenthesized-sexp))
     ((context-coloring-elisp-identifier-p syntax-code)
      (context-coloring-elisp-colorize-symbol))
     ((= syntax-code context-coloring-EXPRESSION-PREFIX-CODE)
      (context-coloring-elisp-colorize-expression-prefix))
     ((= syntax-code context-coloring-STRING-QUOTE-CODE)
      (context-coloring-elisp-colorize-string))
     ((= syntax-code context-coloring-ESCAPE-CODE)
      (forward-char 2)))))

(defun context-coloring-elisp-colorize-comments-and-strings-in-region (start end)
  "Color comments and strings between START and END."
  (let (syntax-code)
    (goto-char start)
    (while (> end (progn (skip-syntax-forward "^\"<\\" end)
                         (point)))
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((= syntax-code context-coloring-STRING-QUOTE-CODE)
        (context-coloring-elisp-colorize-string))
       ((= syntax-code context-coloring-COMMENT-START-CODE)
        (context-coloring-elisp-colorize-comment))
       ((= syntax-code context-coloring-ESCAPE-CODE)
        (forward-char 2))))))

(defun context-coloring-elisp-colorize-region (start end)
  "Color everything between START and END."
  (let (syntax-code)
    (goto-char start)
    (while (> end (progn (skip-syntax-forward "^w_('\"<\\" end)
                         (point)))
      (setq syntax-code (context-coloring-get-syntax-code))
      (cond
       ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
        (context-coloring-elisp-colorize-parenthesized-sexp))
       ((context-coloring-elisp-identifier-p syntax-code)
        (context-coloring-elisp-colorize-symbol))
       ((= syntax-code context-coloring-EXPRESSION-PREFIX-CODE)
        (context-coloring-elisp-colorize-expression-prefix))
       ((= syntax-code context-coloring-STRING-QUOTE-CODE)
        (context-coloring-elisp-colorize-string))
       ((= syntax-code context-coloring-COMMENT-START-CODE)
        (context-coloring-elisp-colorize-comment))
       ((= syntax-code context-coloring-ESCAPE-CODE)
        (forward-char 2))))))

(defun context-coloring-elisp-colorize-region-initially (start end)
  "Begin coloring everything between START and END."
  (setq context-coloring-elisp-sexp-count 0)
  (setq context-coloring-elisp-scope-stack '())
  (let ((inhibit-point-motion-hooks t)
        (case-fold-search nil)
        ;; This is a recursive-descent parser, so give it a big stack.
        (max-lisp-eval-depth (max max-lisp-eval-depth 3000))
        (max-specpdl-size (max max-specpdl-size 3000)))
    (context-coloring-elisp-colorize-region start end)))

(defun context-coloring-elisp-colorize ()
  "Color the current buffer, parsing elisp to determine its
scopes and variables."
  (interactive)
  (with-silent-modifications
    (save-excursion
      (condition-case nil
          (cond
           ;; Just colorize the changed region.
           (context-coloring-changed-p
            (let* (;; Prevent `beginning-of-defun' from making poor assumptions.
                   (open-paren-in-column-0-is-defun-start nil)
                   ;; Seek the beginning and end of the previous and next
                   ;; offscreen defuns, so just enough is colored.
                   (start (progn (goto-char context-coloring-changed-start)
                                 (while (and (< (point-min) (point))
                                             (pos-visible-in-window-p))
                                   (end-of-line 0))
                                 (beginning-of-defun)
                                 (point)))
                   (end (progn (goto-char context-coloring-changed-end)
                               (while (and (> (point-max) (point))
                                           (pos-visible-in-window-p))
                                 (forward-line 1))
                               (end-of-defun)
                               (point))))
              (context-coloring-elisp-colorize-region-initially start end)))
           (t
            (context-coloring-elisp-colorize-region-initially (point-min) (point-max))))
        ;; Scan errors can happen virtually anywhere if parenthesis are
        ;; unbalanced.  Just swallow them.  (`progn' for test coverage.)
        (scan-error (progn))))))


;;; Shell command scopification / colorization

(defun context-coloring-apply-tokens (tokens)
  "Process a string of TOKENS to apply context-based coloring to
the current buffer.  Tokens are 3 integers: start, end, level.  A
new token occurrs after every 3rd element, and the elements are
separated by commas."
  (let* ((tokens (mapcar #'string-to-number (split-string tokens ","))))
    (while tokens
      (context-coloring-colorize-region
       (pop tokens)
       (pop tokens)
       (pop tokens))))
  (context-coloring-colorize-comments-and-strings))

(defun context-coloring-parse-array (array)
  "Parse ARRAY as a flat JSON array of numbers and use the tokens
to colorize the buffer."
  (let* ((braceless (substring-no-properties (context-coloring-trim array) 1 -1)))
    (when (> (length braceless) 0)
      (with-silent-modifications
        (context-coloring-apply-tokens braceless)))))

(defvar-local context-coloring-scopifier-cancel-function nil
  "Kills the current scopification process.")

(defvar-local context-coloring-scopifier-process nil
  "The single scopifier process that can be running.")

(defun context-coloring-cancel-scopification ()
  "Stop the currently-running scopifier from scopifying."
  (when context-coloring-scopifier-cancel-function
    (funcall context-coloring-scopifier-cancel-function)
    (setq context-coloring-scopifier-cancel-function nil))
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-shell-command (command callback)
  "Invoke COMMAND, read its response asynchronously and invoke
CALLBACK with its output.  Return the command process."
  (let ((process (start-process-shell-command "context-coloring-process" nil command))
        (output ""))
    ;; The process may produce output in multiple chunks.  This filter
    ;; accumulates the chunks into a message.
    (set-process-filter
     process
     (lambda (_process chunk)
       (setq output (concat output chunk))))
    ;; When the process's message is complete, this sentinel parses it as JSON
    ;; and applies the tokens to the buffer.
    (set-process-sentinel
     process
     (lambda (_process event)
       (when (equal "finished\n" event)
         (funcall callback output))))
    process))

(defun context-coloring-scopify-shell-command (command callback)
  "Invoke a scopifier via COMMAND, read its response
asynchronously and invoke CALLBACK with its output."
  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-cancel-scopification)
  ;; Start the process.
  (setq context-coloring-scopifier-process
        (context-coloring-shell-command command callback)))

(defun context-coloring-send-buffer-to-scopifier ()
  "Give the scopifier process its input so it can begin
scopifying."
  (process-send-region
   context-coloring-scopifier-process
   (point-min) (point-max))
  (process-send-eof
   context-coloring-scopifier-process))

(defun context-coloring-start-scopifier-server (command host port callback)
  "Connect to or start a scopifier server with COMMAND, HOST and PORT.
Invoke CALLBACK with a network stream when the server is ready
for connections."
  (let* ((connect
          (lambda ()
            (let ((stream (open-network-stream "context-coloring-stream" nil host port)))
              (funcall callback stream)))))
    ;; Try to connect in case a server is running, otherwise start one.
    (condition-case nil
        (progn
          (funcall connect))
      (error
       (let ((server (start-process-shell-command
                      "context-coloring-scopifier-server" nil
                      (context-coloring-join
                       (list command
                             "--server"
                             "--host" host
                             "--port" (number-to-string port))
                       " ")))
             (output ""))
         ;; Connect as soon as the "listening" message is printed.
         (set-process-filter
          server
          (lambda (_process chunk)
            (setq output (concat output chunk))
            (when (string-match-p (format "^Scopifier listening at %s:%s$" host port) output)
              (funcall connect)))))))))

(defun context-coloring-send-buffer-to-scopifier-server (command host port callback)
  "Send the current buffer to the scopifier server running with
COMMAND, HOST and PORT.  Invoke CALLBACK with the server's
response (a stringified JSON array)."
  (context-coloring-start-scopifier-server
   command host port
   (lambda (process)
     (let* ((body (buffer-substring-no-properties (point-min) (point-max)))
            (header (concat "POST / HTTP/1.0\r\n"
                            "Host: localhost\r\n"
                            "Content-Type: application/x-www-form-urlencoded"
                            "; charset=UTF8\r\n"
                            (format "Content-Length: %d\r\n" (length body))
                            "\r\n"))
            (output "")
            (active t))
       (set-process-filter
        process
        (lambda (_process chunk)
          (setq output (concat output chunk))))
       (set-process-sentinel
        process
        (lambda (_process event)
          (when (and (equal "connection broken by remote peer\n" event)
                     active)
            ;; Strip the response headers.
            (string-match "\r\n\r\n" output)
            (setq output (substring-no-properties output (match-end 0)))
            (funcall callback output))))
       (process-send-string process (concat header body "\r\n"))
       (setq context-coloring-scopifier-cancel-function
             (lambda ()
               "Cancel this scopification."
               (setq active nil)))))))

(defun context-coloring-scopify-and-colorize-server (command host port &optional callback)
  "Color the current buffer via the server started with COMMAND,
HOST and PORT.  Invoke CALLBACK when complete."
  (let ((buffer (current-buffer)))
    (context-coloring-send-buffer-to-scopifier-server
     command host port
     (lambda (output)
       (with-current-buffer buffer
         (context-coloring-parse-array output))
       (when callback (funcall callback))))))

(defun context-coloring-scopify-and-colorize (command &optional callback)
  "Color the current buffer via COMMAND.  Invoke CALLBACK when
complete."
  (let ((buffer (current-buffer)))
    (context-coloring-scopify-shell-command
     command
     (lambda (output)
       (with-current-buffer buffer
         (context-coloring-parse-array output))
       (setq context-coloring-scopifier-process nil)
       (when callback (funcall callback)))))
  (context-coloring-send-buffer-to-scopifier))


;;; Dispatch

(defvar context-coloring-dispatch-hash-table (make-hash-table :test #'eq)
  "Map dispatch strategy names to their corresponding property
lists, which contain details about the strategies.")

(defvar context-coloring-mode-hash-table (make-hash-table :test #'eq)
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
and color in a single function (`:colorizer'), parse with a shell
command that returns scope data (`:command'), or parse with a
server that returns scope data (`:command', `:host' and `:port').
In the latter two cases, the scope data will be used to
automatically color the buffer.

PROPERTIES must include `:modes' and one of `:colorizer',
`:scopifier' or `:command'.

`:modes' - List of major modes this dispatch is valid for.

`:colorizer' - Symbol referring to a function that parses and
colors the buffer.

`:executable' - Optional name of an executable required by
`:command'.

`:command' - Shell command to execute with the current buffer
sent via stdin, and with a flat JSON array of start, end and
level data returned via stdout.

`:host' - Hostname of the scopifier server, e.g. \"localhost\".

`:port' - Port number of the scopifier server, e.g. 80, 1337.

`:delay' - Delay between buffer update and colorization, to
override `context-coloring-default-delay'.

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
        (command (plist-get properties :command)))
    (when (null modes)
      (error "No mode defined for dispatch"))
    (when (not (or colorizer
                   command))
      (error "No colorizer or command defined for dispatch"))
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
        (context-coloring-shell-command
         (context-coloring-join (list command "--version") " ")
         (lambda (output)
           (if (context-coloring-check-version version output)
               (progn
                 (when callback (funcall callback t)))
             (when callback (funcall callback nil)))
           (run-hooks 'context-coloring-check-scopifier-version-hook)))))))


;;; Themes

(defvar context-coloring-theme-hash-table (make-hash-table :test #'eq)
  "Map theme names to theme properties.")

(defun context-coloring-theme-p (theme)
  "Return t if THEME is defined, nil otherwise."
  (and (gethash theme context-coloring-theme-hash-table)))

(defconst context-coloring-level-face-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face"
  "Extract a level from a face.")

(defvar context-coloring-originally-set-theme-hash-table
  (make-hash-table :test #'eq)
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
     #'custom-theme-set-faces
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


;;; Built-in dispatches

(context-coloring-define-dispatch
 'javascript-node
 :modes '(js-mode js3-mode)
 :executable "scopifier"
 :command "scopifier"
 :version "v1.2.1"
 :host "localhost"
 :port 6969)

(context-coloring-define-dispatch
 'javascript-js2
 :modes '(js2-mode)
 :colorizer #'context-coloring-js2-colorize
 :setup
 (lambda ()
   (add-hook 'js2-post-parse-callbacks #'context-coloring-colorize nil t))
 :teardown
 (lambda ()
   (remove-hook 'js2-post-parse-callbacks #'context-coloring-colorize t)))

(context-coloring-define-dispatch
 'emacs-lisp
 :modes '(emacs-lisp-mode)
 :colorizer #'context-coloring-elisp-colorize
 :delay 0.016 ;; Thanks to lazy colorization this can be 60 frames per second.
 :setup #'context-coloring-setup-idle-change-detection
 :teardown #'context-coloring-teardown-idle-change-detection)

(defun context-coloring-dispatch (&optional callback)
  "Determine the optimal track for scopification / coloring of
the current buffer, then execute it.

Invoke CALLBACK when complete.  It is invoked synchronously for
elisp tracks, and asynchronously for shell command tracks."
  (let* ((dispatch (context-coloring-get-dispatch-for-mode major-mode))
         (colorizer (plist-get dispatch :colorizer))
         (command (plist-get dispatch :command))
         (host (plist-get dispatch :host))
         (port (plist-get dispatch :port))
         interrupted-p)
    (cond
     (colorizer
      (setq interrupted-p
            (catch 'interrupted
              (funcall colorizer)))
      (when (and (not interrupted-p)
                 callback)
        (funcall callback)))
     (command
      (cond
       ((and host port)
        (context-coloring-scopify-and-colorize-server command host port callback))
       (t
        (context-coloring-scopify-and-colorize command callback)))))))


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

    ;; Safely change the value of this function as necessary.
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
