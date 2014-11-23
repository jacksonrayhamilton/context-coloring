# Context Coloring

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights JavaScript code according to function context.

- Code in the global scope is one color. Code in functions within the global
  scope is a different color, and code within such functions is another color,
  and so on.
- Identifiers retain the color of the scope in which they were declared.
- Identifiers are bold when first declared.
- Comments are gray and italic.

In JavaScript, we are constantly leveraging closures to bind nearby
data. Lexical scope information at-a-glance can assist a programmer in
understanding the overall structure of a program. It can also help curb nasty
bugs like implicit globals and name shadowing, and act as an indicator of
excessive complexity.

There are some breakthrough advantages, too. Context coloring could enable a
programmer to write in a functional style. It would be easy to tell when he had
escaped the boundaries of his function and produced side-effects.

Context coloring also improves a programmer's ability to write functions that
construct objects with implicit private state (which is a good way to avoid
`this` too).

This coloring scheme is probably more useful than conventional JavaScript
*syntax* highlighting. Highlighting keywords can help detect spelling errors, or
alert one to unclosed string literals; but so can a [linter][]. (If you haven't
already, you should [integrate][emacs integration] one into your editor.)

## Usage

Requires Emacs 24+ and [Node.js 0.10+][node].

- Clone this repository.
- Run `make` in it.
- Add it to your [load path][].
- Add the following to your `~/.emacs`:

```lisp
(require 'context-coloring)
(add-hook 'js-mode-hook 'context-coloring-mode)
```

[linter]: https://github.com/jacksonrayhamilton/jslinted
[emacs integration]: https://github.com/jacksonrayhamilton/jslinted#emacs-integration
[node]: http://nodejs.org/download/
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
