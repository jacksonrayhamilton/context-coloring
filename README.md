# Context Coloring

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights JavaScript code according to function context. Code in the global
scope is white. Code in functions within the global scope is yellow, and code
within such functions is green, etc. Identifiers retain the color of the scope
in which they were defined. Comments are gray.

This coloring scheme is probably more useful than conventional JavaScript
*syntax* highlighting. Lexical scope information at-a-glance is probably more
useful than detecting if you spelled "function" wrong, or determing that you
forgot to close a string literal. (A [linter][] would serve you better.)

## Usage

Requires Emacs 24+ and [Node.js 0.10+][node].

- Clone this repository.
- Run `npm install --production` in it.
- Add it to your [load path][].
- Add the following to your `~/.emacs`:

```lisp
(require 'context-coloring)
(add-hook 'js-mode-hook 'context-coloring-mode)
```

[linter]: https://github.com/jacksonrayhamilton/jslinted
[node]: http://nodejs.org/download/
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
