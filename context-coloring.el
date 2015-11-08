;;; context-coloring.el --- Highlight by scope  -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; Version: 7.1.0
;; Keywords: convenience faces tools
;; Package-Requires: ((emacs "24.3") (js2-mode "20150713"))
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


;;; Faces

(defun context-coloring-defface (level light dark tty)
  "Define a face for LEVEL with LIGHT, DARK and TTY colors."
  (let ((face (intern (format "context-coloring-level-%s-face" level)))
        (doc (format "Context coloring face, level %s." level)))
    (custom-declare-face
     face
     `((((type tty)) (:foreground ,tty))
       (((background light)) (:foreground ,light))
       (((background dark)) (:foreground ,dark)))
     doc
     :group 'context-coloring)))

;; Provide some default colors based off Emacs's defaults.
(context-coloring-defface 0 "#000000" "#ffffff" nil)
(context-coloring-defface 1 "#008b8b" "#00ffff" "yellow")
(context-coloring-defface 2 "#0000ff" "#87cefa" "green")
(context-coloring-defface 3 "#483d8b" "#b0c4de" "cyan")
(context-coloring-defface 4 "#a020f0" "#eedd82" "blue")
(context-coloring-defface 5 "#a0522d" "#98fb98" "magenta")
(context-coloring-defface 6 "#228b22" "#7fffd4" "red")
(context-coloring-defface 7 "#3f3f3f" "#cdcdcd" nil)

(defconst context-coloring-default-maximum-face 7
  "Maximum face when there are no custom faces.")

;; Create placeholder faces for users and theme authors.
(dotimes (level 18)
  (let* ((level (+ level 8))
         (face (intern (format "context-coloring-level-%s-face" level)))
         (doc (format "Context coloring face, level %s." level)))
    (custom-declare-face face nil doc :group 'context-coloring)))

(defvar-local context-coloring-maximum-face nil
  "Dynamic index of the highest face available for coloring.")

(defsubst context-coloring-level-face (level)
  "Return symbol for face with LEVEL."
  ;; `concat' is faster than `format' here.
  (intern-soft
   (concat "context-coloring-level-" (number-to-string level) "-face")))

(defsubst context-coloring-bounded-level-face (level)
  "Return symbol for face with LEVEL, bounded by the maximum."
  (context-coloring-level-face (min level context-coloring-maximum-face)))

(defconst context-coloring-level-face-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face"
  "Extract a level from a face.")

(defun context-coloring-theme-highest-level (theme)
  "Return the highest coloring level for THEME, or -1."
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

(defun context-coloring-update-maximum-face ()
  "Save the highest possible face for the current theme."
  (let ((themes (append custom-enabled-themes '(user)))
        (continue t)
        theme
        highest-level)
    (while continue
      (setq theme (car themes))
      (setq themes (cdr themes))
      (setq highest-level (context-coloring-theme-highest-level theme))
      (setq continue (and themes (= highest-level -1))))
    (setq context-coloring-maximum-face
          (cond
           ((= highest-level -1)
            context-coloring-default-maximum-face)
           (t
            highest-level)))))


;;; Change detection

(defvar-local context-coloring-changed-p nil
  "Indication that the buffer has changed recently, which implies
that it should be colored again by
`context-coloring-maybe-colorize-idle-timer' if that timer is
being used.")

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
  (setq context-coloring-changed-start start)
  (setq context-coloring-changed-end end)
  (setq context-coloring-changed-length length)
  (setq context-coloring-changed-p t))

(defun context-coloring-maybe-colorize-with-buffer (buffer)
  "Color BUFFER and if it has changed."
  (when (and (eq buffer (current-buffer))
             context-coloring-changed-p)
    (context-coloring-colorize-with-buffer buffer)
    (setq context-coloring-changed-p nil)
    (setq context-coloring-changed-start nil)
    (setq context-coloring-changed-end nil)
    (setq context-coloring-changed-length nil)))

(defvar-local context-coloring-maybe-colorize-idle-timer nil
  "The currently-running idle timer for conditional coloring.")

(defvar-local context-coloring-colorize-idle-timer nil
  "The currently-running idle timer for unconditional coloring.")

(defcustom context-coloring-default-delay 0.25
  "Default delay between a buffer update and colorization.

Increase this if your machine is high-performing.  Decrease it if
it ain't."
  :type 'float
  :group 'context-coloring)

(make-obsolete-variable
 'context-coloring-delay
 'context-coloring-default-delay
 "6.4.0")

(defun context-coloring-cancel-timer (timer)
  "Cancel TIMER."
  (when timer
    (cancel-timer timer)))

(defun context-coloring-schedule-coloring (time)
  "Schedule coloring to occur once after Emacs is idle for TIME."
  (context-coloring-cancel-timer context-coloring-colorize-idle-timer)
  (setq context-coloring-colorize-idle-timer
        (run-with-idle-timer
         time
         nil
         #'context-coloring-colorize-with-buffer
         (current-buffer))))

(defun context-coloring-setup-idle-change-detection ()
  "Setup idle change detection."
  (let ((dispatch (context-coloring-get-current-dispatch)))
    (add-hook
     'after-change-functions #'context-coloring-change-function nil t)
    (add-hook
     'kill-buffer-hook #'context-coloring-teardown-idle-change-detection nil t)
    (setq context-coloring-maybe-colorize-idle-timer
          (run-with-idle-timer
           (or (plist-get dispatch :delay) context-coloring-default-delay)
           t
           #'context-coloring-maybe-colorize-with-buffer
           (current-buffer)))))

(defun context-coloring-teardown-idle-change-detection ()
  "Teardown idle change detection."
  (dolist (timer (list context-coloring-colorize-idle-timer
                       context-coloring-maybe-colorize-idle-timer))
    (context-coloring-cancel-timer timer))
  (remove-hook
   'kill-buffer-hook #'context-coloring-teardown-idle-change-detection t)
  (remove-hook
   'after-change-functions #'context-coloring-change-function t))


;;; Colorization utilities

(defsubst context-coloring-colorize-region (start end level)
  "Color from START (inclusive) to END (exclusive) with LEVEL."
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
  :type 'boolean
  :group 'context-coloring)

(defcustom context-coloring-syntactic-strings t
  "If non-nil, also color strings using `font-lock'."
  :type 'boolean
  :group 'context-coloring)

(defun context-coloring-font-lock-syntactic-comment-function (state)
  "Color a comment according to STATE."
  (if (nth 3 state) nil font-lock-comment-face))

(defun context-coloring-font-lock-syntactic-string-function (state)
  "Color a string according to STATE."
  (if (nth 3 state) font-lock-string-face nil))

(defsubst context-coloring-colorize-comments-and-strings (&optional min max)
  "Maybe color comments and strings in buffer from MIN to MAX.
MIN defaults to beginning of buffer.  MAX defaults to end."
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

(defcustom context-coloring-initial-level 0
  "Scope level at which to start coloring.

If top-level variables and functions do not become global, but
are scoped to a file (as in Node.js), set this to `1'."
  :type 'integer
  :safe #'integerp
  :group 'context-coloring)


;;; JavaScript colorization

(defvar-local context-coloring-js2-scope-level-hash-table nil
  "Associate `js2-scope' structures and with their scope
  levels.")

(defcustom context-coloring-javascript-block-scopes nil
  "If non-nil, also color block scopes in the scope hierarchy in JavaScript.

The block-scoped `let' and `const' are introduced in ES6.  Enable
this for ES6 code; disable it elsewhere."
  :type 'boolean
  :safe #'booleanp
  :group 'context-coloring)

(make-obsolete-variable
 'context-coloring-js-block-scopes
 'context-coloring-javascript-block-scopes
 "7.0.0")

(defsubst context-coloring-js2-scope-level (scope initial)
  "Return the level of SCOPE, starting from INITIAL."
  (cond ((gethash scope context-coloring-js2-scope-level-hash-table))
        (t
         (let ((level initial)
               (current-scope scope)
               enclosing-scope)
           (while (and current-scope
                       (js2-node-parent current-scope)
                       (setq enclosing-scope
                             (js2-node-get-enclosing-scope current-scope)))
             (when (or context-coloring-javascript-block-scopes
                       (let ((type (js2-scope-type current-scope)))
                         (or (= type js2-SCRIPT)
                             (= type js2-FUNCTION)
                             (= type js2-CATCH))))
               (setq level (+ level 1)))
             (setq current-scope enclosing-scope))
           (puthash scope level context-coloring-js2-scope-level-hash-table)))))

(defsubst context-coloring-js2-local-name-node-p (node)
  "Determine if NODE represents a local variable."
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

(defun context-coloring-js2-colorize-ast ()
  "Color the buffer using the `js2-mode' abstract syntax tree."
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
            (context-coloring-js2-scope-level node context-coloring-initial-level)))
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
                ;; Use `0' as an initial level so global variables are always at
                ;; the highest level (even if `context-coloring-initial-level'
                ;; specifies an initial level for the rest of the code).
                (context-coloring-js2-scope-level defining-scope 0))))))
         ;; The `t' indicates to search children.
         t)))
    (context-coloring-colorize-comments-and-strings)))

(defconst context-coloring-node-comment-regexp
  (concat
   ;; Ensure the "//" or "/*" comment starts with the directive.
   "\\(//[[:space:]]*\\|/\\*[[:space:]]*\\)"
   ;; Support multiple directive formats.
   "\\("
   ;; JSLint and JSHint support a JSON-like format.
   "\\(jslint\\|jshint\\)[[:space:]].*?node:[[:space:]]*true"
   "\\|"
   ;; ESLint just specifies the option name.
   "eslint-env[[:space:]].*?node"
   "\\)")
  "Match a comment body hinting at a Node.js program.")

;; TODO: Add ES6 module detection.
(defun context-coloring-js2-top-level-local-p ()
  "Guess whether top-level variables are local.
For instance, the current file could be a Node.js program."
  (or
   ;; A shebang is a pretty obvious giveaway.
   (string-equal
    "node"
    (save-excursion
      (goto-char (point-min))
      (when (looking-at auto-mode-interpreter-regexp)
        (match-string 2))))
   ;; Otherwise, perform static analysis.
   (progn
     (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test #'eq))
     (catch 'node-program-p
       (js2-visit-ast
        js2-mode-ast
        (lambda (node end-p)
          (when (null end-p)
            (when
                (cond
                 ;; Infer based on inline linter configuration.
                 ((js2-comment-node-p node)
                  (string-match-p
                   context-coloring-node-comment-regexp
                   (js2-node-string node)))
                 ;; Infer based on the prescence of certain variables.
                 ((and (js2-name-node-p node)
                       (let ((parent (js2-node-parent node)))
                         (not (and (js2-object-prop-node-p parent)
                                   (eq node (js2-object-prop-node-left parent))))))
                  (let ((name (js2-name-node-name node))
                        (parent (js2-node-parent node)))
                    (and
                     (cond
                      ;; Check whether this is "exports.something" or
                      ;; "module.exports".
                      ((js2-prop-get-node-p parent)
                       (and
                        (eq node (js2-prop-get-node-left parent))
                        (or (string-equal name "exports")
                            (let* ((property (js2-prop-get-node-right parent))
                                   (property-name (js2-name-node-name property)))
                              (and (string-equal name "module")
                                   (string-equal property-name "exports"))))))
                      ;; Check whether it's a "require('module')" call.
                      ((js2-call-node-p parent)
                       (or (string-equal name "require"))))
                     (let* ((enclosing-scope (js2-node-get-enclosing-scope node))
                            (defining-scope (js2-get-defining-scope
                                             enclosing-scope name)))
                       ;; The variable also must be global.
                       (null defining-scope))))))
              (throw 'node-program-p t))
            ;; The `t' indicates to search children.
            t)))
       ;; Default to returning nil from the catch body.
       nil))))

(defcustom context-coloring-javascript-detect-top-level-scope t
  "If non-nil, detect when to use file-level scope."
  :type 'boolean
  :group 'context-coloring)

(defun context-coloring-js2-colorize ()
  "Color the buffer using the `js2-mode'."
  (cond
   ;; Increase the initial level if we should.
   ((and context-coloring-javascript-detect-top-level-scope
         (context-coloring-js2-top-level-local-p))
    (let ((context-coloring-initial-level 1))
      (context-coloring-js2-colorize-ast)))
   (t
    (context-coloring-js2-colorize-ast))))


;;; Emacs Lisp colorization

(defsubst context-coloring-forward-sws ()
  "Move forward through whitespace and comments."
  (while (forward-comment 1)))

(defsubst context-coloring-elisp-forward-sws ()
  "Move through whitespace and comments, coloring comments."
  (let ((start (point)))
    (context-coloring-forward-sws)
    (context-coloring-colorize-comments-and-strings start (point))))

(defsubst context-coloring-elisp-forward-sexp ()
  "Skip/ignore missing sexps, coloring comments and strings."
  (let ((start (point)))
    (condition-case nil
        (forward-sexp)
      (scan-error (context-coloring-forward-sws)))
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

(defconst context-coloring-elisp-ignored-word-regexp
  (context-coloring-join (list "\\`[-+]?[0-9]"
                               "\\`[&:].+"
                               (context-coloring-exact-or-regexp
                                '("t" "nil" "." "?")))
                         "\\|")
  "Match symbols that can't be bound as variables.")

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
  "Check if SYNTAX-CODE is an elisp identifier constituent."
  (or (= syntax-code context-coloring-WORD-CODE)
      (= syntax-code context-coloring-SYMBOL-CODE)))

(defvar context-coloring-parse-interruptable-p t
  "Set this to nil to force parse to continue until finished.")

(defconst context-coloring-elisp-sexps-per-pause 350
  "Pause after this many iterations to check for user input.
If user input is pending, stop the parse.  This makes for a
smoother user experience for large files.

This number should trigger pausing at about 60 frames per
second.")

(defvar context-coloring-elisp-sexp-count 0
  "Current number of sexps leading up to the next pause.")

(defsubst context-coloring-elisp-increment-sexp-count ()
  "Maybe check if the user interrupted the current parse."
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
  "Return the level of VARIABLE, or 0 if it isn't found."
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
  "Parse the symbol at point.
If the symbol can be bound, invoke CALLBACK with it."
  (let* ((arg-string (buffer-substring-no-properties
                      (point)
                      (progn (context-coloring-elisp-forward-sexp)
                             (point)))))
    (when (not (string-match-p
                context-coloring-elisp-ignored-word-regexp
                arg-string))
      (funcall callback arg-string))))

(defun context-coloring-elisp-parse-let-varlist (type)
  "Parse the list of variable initializers at point.
If TYPE is `let', all the variables are bound after all their
initializers are parsed; if TYPE is `let*', each variable is
bound immediately after its own initializer is parsed."
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

(defun context-coloring-elisp-skip-callee-name ()
  "Skip past the opening parenthesis and name of a function."
  ;; Enter.
  (forward-char)
  (context-coloring-elisp-forward-sws)
  ;; Skip past the function name.
  (forward-sexp)
  (context-coloring-elisp-forward-sws))

(defun context-coloring-elisp-colorize-scope (callback)
  "Color the whole scope at point with its one color.
Handle a header in CALLBACK."
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
    (context-coloring-elisp-skip-callee-name)
    (funcall callback)
    (context-coloring-elisp-colorize-region (point) (1- end))
    ;; Exit.
    (forward-char)
    (context-coloring-elisp-pop-scope)))

(defun context-coloring-elisp-parse-header (callback)
  "Parse a function header at point with CALLBACK."
  (when (= (context-coloring-get-syntax-code) context-coloring-OPEN-PARENTHESIS-CODE)
    (funcall callback)))

(defun context-coloring-elisp-colorize-defun-like (callback)
  "Color the defun-like function at point.
Parse the header with CALLBACK."
  (context-coloring-elisp-colorize-scope
   (lambda ()
     (when (context-coloring-elisp-identifier-p (context-coloring-get-syntax-code))
       ;; Color the defun's name with the top-level color.
       (context-coloring-colorize-region
        (point)
        (progn (forward-sexp)
               (point))
        0)
       (context-coloring-elisp-forward-sws)
       (context-coloring-elisp-parse-header callback)))))

(defun context-coloring-elisp-colorize-defun ()
  "Color the `defun' at point."
  (context-coloring-elisp-colorize-defun-like
   'context-coloring-elisp-parse-arglist))

(defun context-coloring-elisp-colorize-defadvice ()
  "Color the `defadvice' at point."
  (context-coloring-elisp-colorize-defun-like
   (lambda ()
     (let (syntax-code)
       ;; Enter.
       (forward-char)
       (while (/= (setq syntax-code (context-coloring-get-syntax-code))
                  context-coloring-CLOSE-PARENTHESIS-CODE)
         (cond
          ((= syntax-code context-coloring-OPEN-PARENTHESIS-CODE)
           (context-coloring-elisp-parse-arglist))
          (t
           ;; Ignore artifacts.
           (context-coloring-elisp-forward-sexp)))
         (context-coloring-elisp-forward-sws))))))

(defun context-coloring-elisp-colorize-lambda-like (callback)
  "Color the lambda-like function at point.
Parsing the header with CALLBACK."
  (context-coloring-elisp-colorize-scope
   (lambda ()
     (context-coloring-elisp-parse-header callback))))

(defun context-coloring-elisp-colorize-lambda ()
  "Color the `lambda' at point."
  (context-coloring-elisp-colorize-lambda-like
   'context-coloring-elisp-parse-arglist))

(defun context-coloring-elisp-colorize-let ()
  "Color the `let' at point."
  (context-coloring-elisp-colorize-lambda-like
   (lambda ()
     (context-coloring-elisp-parse-let-varlist 'let))))

(defun context-coloring-elisp-colorize-let* ()
  "Color the `let*' at point."
  (context-coloring-elisp-colorize-lambda-like
   (lambda ()
     (context-coloring-elisp-parse-let-varlist 'let*))))

(defun context-coloring-elisp-colorize-macroexp-let2 ()
  "Color the `macroexp-let2' at point."
  (let (syntax-code
        variable)
    (context-coloring-elisp-colorize-scope
     (lambda ()
       (and
        (progn
          (setq syntax-code (context-coloring-get-syntax-code))
          (context-coloring-elisp-identifier-p syntax-code))
        (progn
          (context-coloring-elisp-colorize-sexp)
          (context-coloring-elisp-forward-sws)
          (setq syntax-code (context-coloring-get-syntax-code))
          (context-coloring-elisp-identifier-p syntax-code))
        (progn
          (context-coloring-elisp-parse-bindable
           (lambda (parsed-variable)
             (setq variable parsed-variable)))
          (context-coloring-elisp-forward-sws)
          (when variable
            (context-coloring-elisp-add-variable variable))))))))

(defun context-coloring-elisp-colorize-cond ()
  "Color the `cond' at point."
  (let (syntax-code)
    (context-coloring-elisp-skip-callee-name)
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
  (let (syntax-code
        variable
        case-pos
        case-end)
    (context-coloring-elisp-colorize-scope
     (lambda ()
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
         (context-coloring-elisp-forward-sws))))))

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

(defun context-coloring-elisp-colorize-quote ()
  "Color the `quote' at point."
  (let* ((start (point))
         (end (progn (forward-sexp)
                     (point))))
    (context-coloring-colorize-region
     start
     end
     (context-coloring-elisp-get-current-scope-level))
    (context-coloring-elisp-colorize-comments-and-strings-in-region start end)))

(defvar context-coloring-elisp-callee-dispatch-hash-table
  (let ((table (make-hash-table :test 'equal)))
    (dolist (callee '("defun" "defun*" "defsubst" "defmacro" "cl-defun" "cl-defsubst" "cl-defmacro"))
      (puthash callee #'context-coloring-elisp-colorize-defun table))
    (dolist (callee '("condition-case" "condition-case-unless-debug"))
      (puthash callee #'context-coloring-elisp-colorize-condition-case table))
    (dolist (callee '("dolist" "dotimes"))
      (puthash callee #'context-coloring-elisp-colorize-dolist table))
    (dolist (callee '("let" "gv-letplace"))
      (puthash callee #'context-coloring-elisp-colorize-let table))
    (puthash "let*" #'context-coloring-elisp-colorize-let* table)
    (puthash "macroexp-let2" #'context-coloring-elisp-colorize-macroexp-let2 table)
    (puthash "lambda" #'context-coloring-elisp-colorize-lambda table)
    (puthash "cond" #'context-coloring-elisp-colorize-cond table)
    (puthash "defadvice" #'context-coloring-elisp-colorize-defadvice table)
    (puthash "quote" #'context-coloring-elisp-colorize-quote table)
    (puthash "backquote" #'context-coloring-elisp-colorize-backquote table)
    table)
  "Map function names to their coloring functions.")

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
                             (context-coloring-get-syntax-code)))
         dispatch-function)
    ;; Figure out if the sexp is a special form.
    (cond
     ((and (context-coloring-elisp-identifier-p syntax-code)
           (setq dispatch-function (gethash
                                    (buffer-substring-no-properties
                                     (point)
                                     (progn (forward-sexp)
                                            (point)))
                                    context-coloring-elisp-callee-dispatch-hash-table)))
      (goto-char start)
      (funcall dispatch-function))
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

(defun context-coloring-elisp-colorize-backquote-form ()
  "Color the backquote form at point."
  (let ((start (point))
        (end (progn (forward-sexp)
                    (point)))
        char)
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
     start end)))

(defun context-coloring-elisp-colorize-backquote ()
  "Color the `backquote' at point."
  (context-coloring-elisp-skip-callee-name)
  (context-coloring-elisp-colorize-backquote-form)
  ;; Exit.
  (forward-char))

(defun context-coloring-elisp-colorize-expression-prefix ()
  "Color the expression prefix and expression at point.
It could be a quoted or backquoted expression."
  (context-coloring-elisp-increment-sexp-count)
  (cond
   ((/= (char-after) context-coloring-BACKTICK-CHAR)
    (context-coloring-elisp-forward-sexp))
   (t
    (context-coloring-elisp-colorize-backquote-form))))

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

(defun context-coloring-elisp-colorize-guard (callback)
  "Silently color in CALLBACK."
  (with-silent-modifications
    (save-excursion
      (condition-case nil
          (funcall callback)
        ;; Scan errors can happen virtually anywhere if parenthesis are
        ;; unbalanced.  Just swallow them.  (`progn' for test coverage.)
        (scan-error (progn))))))

(defun context-coloring-elisp-colorize ()
  "Color the current Emacs Lisp buffer."
  (interactive)
  (context-coloring-elisp-colorize-guard
   (lambda ()
     (cond
      ;; Just colorize the changed region.
      (context-coloring-changed-p
       (let* ( ;; Prevent `beginning-of-defun' from making poor assumptions.
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
         (context-coloring-elisp-colorize-region-initially start end)
         ;; Fast coloring is nice, but if the code is not well-formed
         ;; (e.g. an unclosed string literal is parsed at any time) then
         ;; there could be leftover incorrectly-colored code offscreen.  So
         ;; do a clean sweep as soon as appropriate.
         (context-coloring-schedule-coloring context-coloring-default-delay)))
      (t
       (context-coloring-elisp-colorize-region-initially (point-min) (point-max)))))))


;;; eval-expression colorization

(defun context-coloring-eval-expression-match ()
  "Determine expression start in `eval-expression'."
  (string-match "\\`Eval: " (buffer-string)))

(defun context-coloring-eval-expression-colorize ()
  "Color the `eval-expression' minibuffer prompt as elisp."
  (interactive)
  (context-coloring-elisp-colorize-guard
   (lambda ()
     (context-coloring-elisp-colorize-region-initially
      (progn
        (context-coloring-eval-expression-match)
        (1+ (match-end 0)))
      (point-max)))))


;;; Dispatch

(defvar context-coloring-dispatch-hash-table (make-hash-table :test #'eq)
  "Map dispatch strategy names to their property lists.")

(defvar context-coloring-mode-hash-table (make-hash-table :test #'eq)
  "Map major mode names to dispatch property lists.")

(defvar context-coloring-dispatch-predicates '()
  "Functions which may return a dispatch.")

(defun context-coloring-get-current-dispatch ()
  "Return the first dispatch appropriate for the current state."
  (let ((predicates context-coloring-dispatch-predicates)
        (parent major-mode)
        dispatch)
    ;; Maybe a predicate will be satisfied and return a dispatch.
    (while (and predicates
                (not (setq dispatch (funcall (pop predicates))))))
    ;; If not, maybe a major mode (or a derivative) will define a dispatch.
    (when (not dispatch)
      (while (and parent
                  (not (setq dispatch (gethash parent context-coloring-mode-hash-table)))
                  (setq parent (get parent 'derived-mode-parent)))))
    dispatch))

(defun context-coloring-define-dispatch (symbol &rest properties)
  "Define a new dispatch named SYMBOL with PROPERTIES.

A \"dispatch\" is a property list describing a strategy for
coloring a buffer.

PROPERTIES must include one of `:modes' or `:predicate', and a
`:colorizer'.

`:modes' - List of major modes this dispatch is valid for.

`:predicate' - Function that determines if the dispatch is valid
for any given state.

`:colorizer' - Function that parses and colors the buffer.

`:delay' - Delay between buffer update and colorization, to
override `context-coloring-default-delay'.

`:setup' - Arbitrary code to set up this dispatch when
`context-coloring-mode' is enabled.

`:teardown' - Arbitrary code to tear down this dispatch when
`context-coloring-mode' is disabled."
  (let ((modes (plist-get properties :modes))
        (predicate (plist-get properties :predicate))
        (colorizer (plist-get properties :colorizer)))
    (when (null (or modes predicate))
      (error "No mode or predicate defined for dispatch"))
    (when (not colorizer)
      (error "No colorizer defined for dispatch"))
    (puthash symbol properties context-coloring-dispatch-hash-table)
    (dolist (mode modes)
      (puthash mode properties context-coloring-mode-hash-table))
    (when predicate
      (push (lambda ()
              (when (funcall predicate)
                properties)) context-coloring-dispatch-predicates))))

(defun context-coloring-dispatch ()
  "Determine how to color the current buffer, and color it."
  (let* ((dispatch (context-coloring-get-current-dispatch))
         (colorizer (plist-get dispatch :colorizer)))
    (catch 'interrupted
      (funcall colorizer))))


;;; Colorization

(defun context-coloring-colorize ()
  "Color the current buffer by function context."
  (interactive)
  (context-coloring-update-maximum-face)
  (context-coloring-dispatch))

(defun context-coloring-colorize-with-buffer (buffer)
  "Color BUFFER."
  ;; Don't select deleted buffers.
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (context-coloring-colorize))))


;;; Built-in dispatches

(context-coloring-define-dispatch
 'javascript
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

;; `eval-expression-minibuffer-setup-hook' is not available in Emacs 24.3, so
;; the backwards-compatible recommendation is to use `minibuffer-setup-hook' and
;; rely on this predicate instead.
(defun context-coloring-eval-expression-predicate ()
  "Non-nil if the minibuffer is for `eval-expression'."
  ;; Kinda better than checking `this-command', because `this-command' changes.
  (context-coloring-eval-expression-match))

(context-coloring-define-dispatch
 'eval-expression
 :predicate #'context-coloring-eval-expression-predicate
 :colorizer #'context-coloring-eval-expression-colorize
 :delay 0.016
 :setup #'context-coloring-setup-idle-change-detection
 :teardown #'context-coloring-teardown-idle-change-detection)

(defvar context-coloring-ignore-unavailable-predicates
  (list
   #'minibufferp)
  "Cases when \"unavailable\" messages are silenced.
Necessary in editing states where coloring is only sometimes
permissible.")

(defun context-coloring-ignore-unavailable-message-p ()
  "Determine if the unavailable message should be silenced."
  (let ((predicates context-coloring-ignore-unavailable-predicates)
        (ignore-p nil))
    (while (and predicates
                (not ignore-p))
      (setq ignore-p (funcall (pop predicates))))
    ignore-p))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Toggle contextual code coloring.
With a prefix argument ARG, enable Context Coloring mode if ARG
is positive, and disable it otherwise.  If called from Lisp,
enable the mode if ARG is omitted or nil.

Context Coloring mode is a buffer-local minor mode.  When
enabled, code is colored by scope.  Scopes are colored
hierarchically.  Variables referenced from nested scopes retain
the color of their defining scopes.  Certain syntax, like
comments and strings, is still colored with `font-lock'.

The entire buffer is colored initially.  Changes to the buffer
trigger recoloring.

Define your own colors by customizing faces like
`context-coloring-level-N-face', where N is a number starting
from 0.  If no face is found on a custom theme nor the `user'
theme, the defaults are used.

New language / major mode support can be added with
`context-coloring-define-dispatch', which see.

Feature inspired by Douglas Crockford."
  nil " Context" nil
  (cond
   (context-coloring-mode
    ;; Font lock is incompatible with this mode; the converse is also true.
    (font-lock-mode 0)
    (jit-lock-mode nil)
    ;; ...but we do use font-lock functions here.
    (font-lock-set-defaults)
    ;; Safely change the value of this function as necessary.
    (make-local-variable 'font-lock-syntactic-face-function)
    (let ((dispatch (context-coloring-get-current-dispatch)))
      (cond
       (dispatch
        (let ((setup (plist-get dispatch :setup)))
          (when setup
            (funcall setup))
          ;; Colorize once initially.
          (let ((context-coloring-parse-interruptable-p nil))
            (context-coloring-colorize))))
       ((not (context-coloring-ignore-unavailable-message-p))
        (message "Context coloring is unavailable here")))))
   (t
    (let ((dispatch (context-coloring-get-current-dispatch)))
      (when dispatch
        (let ((teardown (plist-get dispatch :teardown)))
          (when teardown
            (funcall teardown)))))
    (font-lock-mode)
    (jit-lock-mode t))))

(provide 'context-coloring)

;;; context-coloring.el ends here
