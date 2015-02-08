;;; context-coloring.el --- Syntax highlighting, except not for syntax. -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2015  Free Software Foundation, Inc.

;; Author: Jackson Ray Hamilton <jackson@jacksonrayhamilton.com>
;; URL: https://github.com/jacksonrayhamilton/context-coloring
;; Keywords: context coloring syntax highlighting
;; Version: 4.1.0
;; Package-Requires: ((emacs "24") (js2-mode "20150126"))

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

;; Highlights code according to function context.

;; - Code in the global scope is one color.  Code in functions within the global
;;   scope is a different color, and code within such functions is another
;;   color, and so on.
;; - Identifiers retain the color of the scope in which they are declared.

;; Lexical scope information at-a-glance can assist a programmer in
;; understanding the overall structure of a program.  It can help to curb nasty
;; bugs like name shadowing.  A rainbow can indicate excessive complexity. State
;; change within a closure is easily monitored.

;; By default, Context Coloring still highlights comments and strings
;; syntactically.  It is still easy to differentiate code from non-code, and
;; strings cannot be confused for variables.

;; To use, add the following to your ~/.emacs:

;; (require 'context-coloring)
;; (add-hook 'js2-mode-hook 'context-coloring-mode)

;; js-mode or js3-mode support requires Node.js 0.10+ and the scopifier
;; executable.

;; $ npm install -g scopifier

;;; Code:

(require 'js2-mode)


;;; Customizable options

(defcustom context-coloring-delay 0.25
  "Delay between a buffer update and colorization.

Increase this if your machine is high-performing.  Decrease it if
it ain't.

Supported modes: `js-mode', `js3-mode'"
  :group 'context-coloring)

(defcustom context-coloring-comments-and-strings t
  "If non-nil, also color comments and strings using `font-lock'."
  :group 'context-coloring)

(defcustom context-coloring-js-block-scopes nil
  "If non-nil, also color block scopes in the scope hierarchy in JavaScript.

The block-scoped `let' and `const' are introduced in ES6.  If you
are writing ES6 code, enable this; otherwise, don't.

Supported modes: `js2-mode'"
  :group 'context-coloring)

(defcustom context-coloring-benchmark-colorization nil
  "If non-nil, track how long colorization takes and print
messages with the colorization duration."
  :group 'context-coloring)


;;; Local variables

(defvar-local context-coloring-buffer nil
  "Reference to this buffer (for timers).")

(defvar-local context-coloring-scopifier-process nil
  "Reference to the single scopifier process that can be
  running.")

(defvar-local context-coloring-colorize-idle-timer nil
  "Reference to the currently-running idle timer.")

(defvar-local context-coloring-changed nil
  "Indication that the buffer has changed recently, which would
imply that it should be colorized again by
`context-coloring-colorize-idle-timer' if that timer is being
used.")


;;; Faces

(defun context-coloring-defface (level tty light dark)
  "Dynamically define a face for LEVEL with colors for TTY, LIGHT
and DARK backgrounds."
  (let ((face (intern (format "context-coloring-level-%s-face" level)))
        (doc (format "Context coloring face, level %s." level)))
    (eval
     (macroexpand
      `(defface ,face
         '((((type tty)) (:foreground ,tty))
           (((background light)) (:foreground ,light))
           (((background dark)) (:foreground ,dark)))
         ,doc
         :group 'context-coloring)))))

(defvar context-coloring-face-count nil
  "Number of faces available for coloring.")

(defun context-coloring-defface-default (level)
  "Define a face for LEVEL with the default neutral colors."
  (context-coloring-defface level nil "#3f3f3f" "#cdcdcd"))

(defun context-coloring-set-colors-default ()
  (context-coloring-defface 0 nil       "#000000" "#ffffff")
  (context-coloring-defface 1 "yellow"  "#007f80" "#ffff80")
  (context-coloring-defface 2 "green"   "#001580" "#cdfacd")
  (context-coloring-defface 3 "cyan"    "#550080" "#d8d8ff")
  (context-coloring-defface 4 "blue"    "#802b00" "#e7c7ff")
  (context-coloring-defface 5 "magenta" "#6a8000" "#ffcdcd")
  (context-coloring-defface 6 "red"     "#008000" "#ffe390")
  (context-coloring-defface-default 7)
  (setq context-coloring-face-count 8))

(context-coloring-set-colors-default)

;; Color theme authors can have up to 26 levels: 1 (0th) for globals, 24
;; (1st-24th) for in-betweens, and 1 (25th) for infinity.
(dotimes (number 18)
  (context-coloring-defface-default (+ number context-coloring-face-count)))


;;; Face functions

(defsubst context-coloring-face-symbol (level)
  "Returns a symbol for a face with LEVEL."
  ;; `concat' is faster than `format' here.
  (intern-soft (concat "context-coloring-level-"
                       (number-to-string level)
                       "-face")))

(defun context-coloring-set-colors (&rest colors)
  "Set context coloring's levels' coloring to COLORS, where the
Nth element of COLORS is level N's color."
  (setq context-coloring-face-count (length colors))
  (let ((level 0))
    (dolist (color colors)
      ;; Ensure there are available faces to contain new colors.
      (when (not (context-coloring-face-symbol level))
        (context-coloring-defface-default level))
      (set-face-foreground (context-coloring-face-symbol level) color)
      (setq level (+ level 1)))))

(defsubst context-coloring-level-face (level)
  "Returns the face name for LEVEL."
  (context-coloring-face-symbol (min level context-coloring-face-count)))


;;; Colorization utilities

(defsubst context-coloring-colorize-region (start end level)
  "Colorizes characters from the 1-indexed START (inclusive) to
END (exclusive) with the face corresponding to LEVEL."
  (add-text-properties
   start
   end
   `(face ,(context-coloring-level-face level))))

(defsubst context-coloring-maybe-colorize-comments-and-strings ()
  "Colorizes the current buffer's comments and strings if
`context-coloring-comments-and-strings' is non-nil."
  (when context-coloring-comments-and-strings
    (save-excursion
      (font-lock-fontify-syntactically-region (point-min) (point-max)))))


;;; js2-mode colorization

(defvar-local context-coloring-js2-scope-level-hash-table nil
  "Associates `js2-scope' structures and with their scope
  levels.")

(defsubst context-coloring-js2-scope-level (scope)
  "Gets the level of SCOPE."
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
  "Determines if NODE is a js2-name-node representing a local
variable."
  (and (js2-name-node-p node)
       (let ((parent (js2-node-parent node)))
         (not (or (and (js2-object-prop-node-p parent)
                       (eq node (js2-object-prop-node-left parent)))
                  (and (js2-prop-get-node-p parent)
                       ;; For nested property lookup, the node on the left is a
                       ;; `js2-prop-get-node', so this always works.
                       (eq node (js2-prop-get-node-right parent))))))))

(defsubst context-coloring-js2-colorize-node (node level)
  "Colors NODE with the color for LEVEL."
  (let ((start (js2-node-abs-pos node)))
    (context-coloring-colorize-region
     start
     (+ start (js2-node-len node)) ; End
     level)))

(defun context-coloring-js2-colorize ()
  "Colorizes the current buffer using the abstract syntax tree
generated by js2-mode."
  ;; Reset the hash table; the old one could be obsolete.
  (setq context-coloring-js2-scope-level-hash-table (make-hash-table :test 'eq))
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


;;; Shell command scopification / colorization

(defun context-coloring-apply-tokens (tokens)
  "Processes a vector of TOKENS to apply context-based coloring
to the current buffer.  Tokens are 3 integers: start, end, level.
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

(defun context-coloring-parse-array (input)
  "Specialized JSON parser for a flat array of numbers."
  (vconcat (mapcar 'string-to-number (split-string (substring input 1 -1) ","))))

(defun context-coloring-kill-scopifier ()
  "Kills the currently-running scopifier process for this
buffer."
  (when (not (null context-coloring-scopifier-process))
    (delete-process context-coloring-scopifier-process)
    (setq context-coloring-scopifier-process nil)))

(defun context-coloring-scopify-shell-command (command &optional callback)
  "Invokes a scopifier with the current buffer's contents,
reading the scopifier's response asynchronously and applying a
parsed list of tokens to `context-coloring-apply-tokens'.

Invokes CALLBACK when complete."

  ;; Prior running tokenization is implicitly obsolete if this function is
  ;; called.
  (context-coloring-kill-scopifier)

  ;; Start the process.
  (setq context-coloring-scopifier-process
        (start-process-shell-command "scopifier" nil command))

  (let ((output "")
        (buffer context-coloring-buffer))

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
         (let ((tokens (context-coloring-parse-array output)))
           (with-current-buffer buffer
             (context-coloring-apply-tokens tokens))
           (setq context-coloring-scopifier-process nil)
           (if callback (funcall callback)))))))

  ;; Give the process its input so it can begin.
  (process-send-region context-coloring-scopifier-process (point-min) (point-max))
  (process-send-eof context-coloring-scopifier-process))


;;; Dispatch

(defvar context-coloring-dispatch-hash-table (make-hash-table :test 'eq)
  "Mapping of dispatch strategy names to their corresponding
  property lists, which contain details about the strategies.")

(defvar context-coloring-mode-hash-table (make-hash-table :test 'eq)
  "Mapping of major mode names to dispatch property lists.")

(defun context-coloring-select-dispatch (mode dispatch)
  "Use DISPATCH for MODE."
  (puthash
   mode
   (gethash
    dispatch
    context-coloring-dispatch-hash-table)
   context-coloring-mode-hash-table))

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
level data returned via stdout."
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
      (when (null (gethash mode context-coloring-mode-hash-table))
        (puthash mode properties context-coloring-mode-hash-table)))))

(context-coloring-define-dispatch
 'javascript-node
 :modes '(js-mode js3-mode)
 :executable "scopifier"
 :command "scopifier")

(context-coloring-define-dispatch
 'javascript-js2
 :modes '(js2-mode)
 :colorizer 'context-coloring-js2-colorize)

(defun context-coloring-dispatch (&optional callback)
  "Determines the optimal track for scopification / colorization
of the current buffer, then executes it.

Invokes CALLBACK when complete.  It is invoked synchronously for
elisp tracks, and asynchronously for shell command tracks."
  (let ((dispatch (gethash major-mode context-coloring-mode-hash-table)))
    (if (null dispatch)
        (message "%s" "Context coloring is not available for this major mode"))
    (let (colorizer
          scopifier
          command
          executable)
      (cond
       ((setq colorizer (plist-get dispatch :colorizer))
        (funcall colorizer)
        (if callback (funcall callback)))
       ((setq scopifier (plist-get dispatch :scopifier))
        (context-coloring-apply-tokens (funcall scopifier))
        (if callback (funcall callback)))
       ((setq command (plist-get dispatch :command))
        (setq executable (plist-get dispatch :executable))
        (if (and executable
                 (null (executable-find executable)))
            (message "Executable \"%s\" not found" executable)
          (context-coloring-scopify-shell-command command callback)))))))


;;; Colorization

(defun context-coloring-colorize (&optional callback)
  "Colors the current buffer by function context.

Invokes CALLBACK when complete; see `context-coloring-dispatch'."
  (interactive)
  (let ((start-time (float-time)))
    (context-coloring-dispatch
     (lambda ()
       (when context-coloring-benchmark-colorization
         (message "Colorization took %.3f seconds" (- (float-time) start-time)))
       (if callback (funcall callback))))))

(defun context-coloring-change-function (_start _end _length)
  "Registers a change so that a buffer can be colorized soon."
  ;; Tokenization is obsolete if there was a change.
  (context-coloring-kill-scopifier)
  (setq context-coloring-changed t))

(defun context-coloring-maybe-colorize ()
  "Colorize unders certain conditions.  This will run as an idle
timer, so firstly the buffer must not be some other buffer.
Additionally, the buffer must have changed, otherwise colorizing
would be redundant."
  (when (and (eq context-coloring-buffer (window-buffer (selected-window)))
             context-coloring-changed)
    (setq context-coloring-changed nil)
    (context-coloring-colorize)))


;;; Themes

(defvar context-coloring-theme-hash-table (make-hash-table :test 'eq)
  "Mapping of theme names to theme properties.")

(defun context-coloring-themep (theme)
  "Return t if THEME is defined, nil otherwise."
  (and (gethash theme context-coloring-theme-hash-table)))

(defconst context-coloring-level-face-regexp
  "context-coloring-level-\\([[:digit:]]+\\)-face"
  "Regular expression for extracting a level from a face.")

(defun context-coloring-theme-definedp (theme)
  "Return t if there is a `context-coloring-level-N-face' defined
for THEME, nil otherwise."
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
    found))

(defun context-coloring-warn-theme-defined (theme)
  "Warns the user that the colors for a theme are already defined."
  (warn "Context coloring colors for theme `%s' are already defined" theme))

(defun context-coloring-theme-highest-level (theme)
  "Return the highest level N of a face like
`context-coloring-level-N-face' defined for THEME, or -1 if there
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
  "Applies THEME's properties to its respective custom theme,
which must already exist and which *should* already be enabled."
  (let* ((properties (gethash theme context-coloring-theme-hash-table))
         (colors (plist-get properties :colors))
         (level -1))
    (setq context-coloring-face-count (length colors))
    (apply
     'custom-theme-set-faces
     theme
     (mapcar
      (lambda (color)
        (setq level (+ level 1))
        `(,(context-coloring-face-symbol level) ((t (:foreground ,color)))))
      colors))))

(defun context-coloring-define-theme (theme &rest properties)
  "Define a theme named THEME for coloring scope levels.

PROPERTIES is a property list specifiying the following details:

`:aliases': List of symbols of other custom themes that these
colors are applicable to.

`:colors': List of colors that this theme uses.

`:override': If non-nil, this theme is intentionally overriding
colors set by a custom theme.  Don't set this non-nil unless
there is a theme you want to use which sets
`context-coloring-level-N-face' faces that you want to replace.

`:recede': If non-nil, this theme should not apply its colors if
a custom theme already sets `context-coloring-level-N-face'
faces.  This option is optimistic; set this non-nil if you would
rather confer the duty of picking colors to a theme author (if /
when he ever gets around to it).

By default, themes will always override custom themes, even if
those custom themes set `context-coloring-level-N-face' faces.
If a theme does override a custom theme, a warning will be
raised, at which point you may want to enable the `:override'
option, or just delete your theme and opt to use your custom
theme's author's colors instead."
  (let ((aliases (plist-get properties :aliases))
        (override (plist-get properties :override))
        (recede (plist-get properties :recede)))
    (dolist (name (append `(,theme) aliases))
      (when (and (not override)
                 (context-coloring-theme-definedp name))
        (context-coloring-warn-theme-defined name))
      (puthash name properties context-coloring-theme-hash-table)
      ;; Set (or overwrite) colors.
      (when (and (custom-theme-p name)
                 (not recede))
        (context-coloring-apply-theme name)))))

(defun context-coloring-load-theme (&optional rest)
  (declare
   (obsolete
    "themes are now loaded alongside custom themes automatically."
    "4.1.0")))

(defun context-coloring-enable-theme (theme)
  "Applies THEME if its colors are not already defined, else just
sets `context-coloring-face-count' to the correct value for
THEME."
  (let* ((properties (gethash theme context-coloring-theme-hash-table))
         (recede (plist-get properties :recede))
         (override (plist-get properties :override)))
    (cond
     (recede
      (let ((highest-level (context-coloring-theme-highest-level theme)))
        (cond
         ((> highest-level -1)
          (setq context-coloring-face-count (+ highest-level 1)))
         (t
          (context-coloring-apply-theme theme)))))
     (t
      (let ((defined (context-coloring-theme-definedp theme)))
        (when (and defined
                   (not override))
          (context-coloring-warn-theme-defined theme))
        (context-coloring-apply-theme theme))))))

(defadvice enable-theme (after context-coloring-enable-theme (theme) activate)
  "Enable colors for themes just-in-time.  We can't set faces for
themes that might not exist yet."
  (when (and (not (eq theme 'user))          ; Called internally by `enable-theme'.
             (context-coloring-themep theme)
             (custom-theme-p theme))         ; Guard against non-existent themes.
    (context-coloring-enable-theme theme)))

(context-coloring-define-theme
 'leuven
 :recede t
 :colors '("#333333"
           "#0000FF"
           "#6434A3"
           "#BA36A5"
           "#D0372D"
           "#036A07"
           "#006699"
           "#006FE0"
           "#808080"))

(context-coloring-define-theme
 'monokai
 :recede t
 :colors '("#F8F8F2"
           "#66D9EF"
           "#A1EFE4"
           "#A6E22E"
           "#E6DB74"
           "#FD971F"
           "#F92672"
           "#FD5FF0"
           "#AE81FF"))

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
           "#69B7F0"
           "#69CABF"
           "#B4C342"
           "#DEB542"
           "#F2804F"
           "#FF6E64"
           "#F771AC"
           "#9EA0E5"))

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
 :colors '("#DCDCCC"
           "#93E0E3"
           "#BFEBBF"
           "#F0DFAF"
           "#DFAF8F"
           "#CC9393"
           "#DC8CC3"
           "#94BFF3"
           "#9FC59F"
           "#D0BF8F"
           "#DCA3A3"))


;;; Minor mode

;;;###autoload
(define-minor-mode context-coloring-mode
  "Context-based code coloring, inspired by Douglas Crockford."
  nil " Context" nil
  (if (not context-coloring-mode)
      (progn
        (context-coloring-kill-scopifier)
        (when context-coloring-colorize-idle-timer
          (cancel-timer context-coloring-colorize-idle-timer))
        (remove-hook 'js2-post-parse-callbacks 'context-coloring-colorize t)
        (remove-hook 'after-change-functions 'context-coloring-change-function t)
        (font-lock-mode)
        (jit-lock-mode t))

    ;; Remember this buffer. This value should not be dynamically-bound.
    (setq context-coloring-buffer (current-buffer))

    ;; Font lock is incompatible with this mode; the converse is also true.
    (font-lock-mode 0)
    (jit-lock-mode nil)

    ;; Colorize once initially.
    (context-coloring-colorize)

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
            (run-with-idle-timer
             context-coloring-delay
             t
             'context-coloring-maybe-colorize)))))

(provide 'context-coloring)

;; Local Variables:
;; eval: (when (fboundp 'rainbow-mode) (rainbow-mode 1))
;; End:

;;; context-coloring.el ends here
