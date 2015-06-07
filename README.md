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
  - `defun`, `lambda`, `let`, `let*`, quotes, backticks, commas.
  - 25,000 lines per second!

## Installation

Requires Emacs 24+.

JavaScript language support requires either [js2-mode][], or
[Node.js 0.10+][node] and the [scopifier][] executable.

### ELPA

- `M-x package-install RET context-coloring RET`

### Git

- Clone this repository.

```bash
cd ~/.emacs.d/
git clone https://github.com/jacksonrayhamilton/context-coloring.git
```

- Byte-compile the package for improved speed.

```bash
cd context-coloring/
make compile
```

- Add the following to your init file:

```lisp
(add-to-list 'load-path "~/.emacs.d/context-coloring")
(require 'context-coloring)
```

### Dependencies (js-mode)

```bash
npm install -g scopifier
```

## Usage

Add the following to your init file:

```lisp
;; js-mode:
(add-hook 'js-mode-hook 'context-coloring-mode)

;; js2-mode:
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'context-coloring-mode)

;; emacs-lisp-mode:
(add-hook 'emacs-lisp-mode-hook 'context-coloring-mode)
```

## Customizing

### Options

- `context-coloring-syntactic-comments` (default: `t`): If non-nil, also color
  comments using `font-lock`.
- `context-coloring-syntactic-strings` (default: `t`): If non-nil, also color
  strings using `font-lock`.
- `context-coloring-default-delay` (default: `0.25`; supported modes: `js-mode`,
  `js3-mode`): Default (sometimes overridden) delay between a buffer update and
  colorization.
- `context-coloring-js-block-scopes` (default: `nil`; supported modes:
  `js2-mode`): If non-nil, also color block scopes in the scope hierarchy in
  JavaScript.

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

## Extending

To add support for a new language, write a "scopifier" for it, and define a new
coloring dispatch strategy with `context-coloring-define-dispatch`.  Then the
plugin should handle the rest.  (See `C-h f context-coloring-define-dispatch`
for more info on dispatch strategies.)

A "scopifier" is a CLI program that reads a buffer's contents from stdin and
writes a JSON array of numbers to stdout.  Every three numbers in the array
represent a range of color.  For instance, if I fed the following string of
JavaScript code to a scopifier

```js
var a = function () {};
```

then the scopifier would produce the following array

```js
[1,24,0,9,23,1]
```

where, for every three numbers, the first number is a 1-indexed start [point][],
the second number is an exclusive end point, and the third number is a scope
level.  The result of applying level 0 coloring to the range &#91;1, 24) and
then applying level 1 coloring to the range &#91;9, 23) would result in the
following coloring:

<p align="center">
  <img alt="Screenshot of ranges &#91;1, 24) and &#91;9, 23)." src="scopifier.png" title="Screenshot">
</p>

If there is an abstract syntax tree generator for your language, you can walk
the syntax tree, find variables and scopes, and build their positions and levels
into an array like the one above.

For example, a Ruby scopifier might be defined and implemented like this:

```lisp
(context-coloring-define-dispatch
 'ruby
 :modes '(ruby-mode)
 :executable "ruby"
 :command "/home/username/scopifier")
```

```ruby
#!/usr/bin/env ruby
def scopifier(code)
    # Parse code.
    # Return an array.
end
print scopifier ARGF.read
```

When a `--version` argument is passed, a scopifier should print its version
number and exit.  This allows context-coloring to determine if an update is
required.

Alternatively, you could implement a "colorizer" in Emacs Lisp.  A colorizer
also handles the job of calling `context-coloring-colorize-region` to apply
colors to a buffer.  A colorizer may have better performance than a scopifier
when parsing and coloring can be performed in the same pass.

[js2-mode]: https://github.com/mooz/js2-mode
[node]: http://nodejs.org/download/
[scopifier]: https://github.com/jacksonrayhamilton/scopifier
[point]: http://www.gnu.org/software/emacs/manual/html_node/elisp/Point.html
