# Context Coloring

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights JavaScript code according to function context. Code in the global
scope is white, code in functions within the global scope is yellow, code within
such functions is green, etc. Comments are grey.

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

[node]: http://nodejs.org/download/
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
