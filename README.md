# Context Coloring

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights JavaScript code according to function context. Code in the global
scope is white, code in functions within the global scope is yellow, code within
such functions is green, etc.

## Usage

- Install [Node.js 0.10][node] (or higher).
- Clone this repository, and run `npm install` in it.
- Put this repository on your [load path][].
- Add the following to your `~/.emacs`:

```lisp
(require 'context-coloring)
(add-hook 'js-mode-hook 'context-coloring-mode)
```

[node]: http://nodejs.org/download/
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
