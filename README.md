# Context Coloring [![Build Status](https://travis-ci.org/jacksonrayhamilton/context-coloring.png?branch=master)](https://travis-ci.org/jacksonrayhamilton/context-coloring) [![Coverage Status](https://coveralls.io/repos/jacksonrayhamilton/context-coloring/badge.svg?branch=master)](https://coveralls.io/r/jacksonrayhamilton/context-coloring?branch=master)

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights code by scope.  Top-level scopes are one color, second-level scopes
are another color, and so on.  Variables retain the color of the scope in which
they are defined.  A variable defined in an outer scope referenced by an inner
scope is colored the same as the outer scope.

By default, comments and strings are still highlighted syntactically.

## Features

- Light and dark (customizable) color schemes.
- JavaScript support:
  - Script, function and block scopes (and even `catch` block scopes).
  - Very fast for files under 1000 lines.
- Emacs Lisp support:
  - `defun`, `lambda`, `let`, `let*`, `cond`, `condition-case`, `defadvice`,
    `dolist`, `quote`, `backquote` and backquote splicing.
  - Instantaneous lazy coloring, 8000 lines-per-second full coloring.
  - Works in `eval-expression` too.

## Installation

Requires Emacs 24.3+.  JavaScript language support requires
[js2-mode](https://github.com/mooz/js2-mode).

`M-x package-install RET context-coloring RET` and add the following to your
init file:

```lisp
;; JavaScript:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'context-coloring-mode)

;; Emacs Lisp:
(add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)

;; eval-expression:
(add-hook 'minibuffer-setup-hook #'context-coloring-mode)
```

## Customizing

### Options

- `context-coloring-syntactic-comments` (default: `t`): If non-nil, also color
  comments using `font-lock`.
- `context-coloring-syntactic-strings` (default: `t`): If non-nil, also color
  strings using `font-lock`.
- `context-coloring-default-delay` (default: `0.25`): Default delay between a
  buffer update and colorization.
- `context-coloring-javascript-block-scopes` (default: `nil`): If non-nil, also
  color block scopes in the scope hierarchy in JavaScript.

### Color Schemes

Color schemes for custom themes are automatically applied when those themes are
active.  Built-in theme support is available for: `ample`, `anti-zenburn`,
`grandshell`, `leuven`, `monokai`, `solarized`, `spacegray`, `tango` and
`zenburn`.

You can define your own theme colors too:

```lisp
(context-coloring-define-theme
 'zenburn
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
```

See `C-h f context-coloring-define-theme` for more info on theme parameters.
