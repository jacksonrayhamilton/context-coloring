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

- JavaScript support:
  - Script, function and block scopes (and even `catch` block scopes).
- Emacs Lisp support:
  - `defun`, `lambda`, `let`, `let*`, `cond`, `condition-case`, `defadvice`,
    `dolist`, `quote`, `backquote` and backquote splicing.
  - Works in `eval-expression` too.

## Installation

Requires Emacs 24.3+.  JavaScript language support requires
[js2-mode](https://github.com/mooz/js2-mode).

To install, run the command `M-x package-install RET context-coloring RET`, and
then add the following to your init file:

```lisp
;; JavaScript:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'context-coloring-mode)

;; Emacs Lisp:
(add-hook 'emacs-lisp-mode-hook #'context-coloring-mode)

;; eval-expression:
(add-hook 'minibuffer-setup-hook #'context-coloring-mode)
```

## Color Schemes

There is *no default color scheme*.  Define the colors according to your liking
by setting the appropriate custom faces and the maximum face:

```lisp
(custom-theme-set-faces
 'zenburn
 '(context-coloring-level-0-face  ((t :foreground "#dcdccc")))
 '(context-coloring-level-1-face  ((t :foreground "#93e0e3")))
 '(context-coloring-level-2-face  ((t :foreground "#bfebbf")))
 '(context-coloring-level-3-face  ((t :foreground "#f0dfaf")))
 '(context-coloring-level-4-face  ((t :foreground "#dfaf8f")))
 '(context-coloring-level-5-face  ((t :foreground "#cc9393")))
 '(context-coloring-level-6-face  ((t :foreground "#dc8cc3")))
 '(context-coloring-level-7-face  ((t :foreground "#94bff3")))
 '(context-coloring-level-8-face  ((t :foreground "#9fc59f")))
 '(context-coloring-level-9-face  ((t :foreground "#d0bf8f")))
 '(context-coloring-level-10-face ((t :foreground "#dca3a3"))))
(setq context-coloring-maximum-face 10)
```

[See here](https://gist.github.com/jacksonrayhamilton/6b89ca3b85182c490816) for
some color schemes for popular custom themes.

## Options

- `context-coloring-syntactic-comments` (default: `t`): If non-nil, also color
  comments using `font-lock`.
- `context-coloring-syntactic-strings` (default: `t`): If non-nil, also color
  strings using `font-lock`.
- `context-coloring-javascript-block-scopes` (default: `nil`): If non-nil, also
  color block scopes in the scope hierarchy in JavaScript.
