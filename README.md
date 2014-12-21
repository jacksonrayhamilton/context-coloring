# Context Coloring

<p align="center">
  <img alt="Screenshot of JavaScript code highlighted by context." src="screenshot.png" title="Screenshot">
</p>

Highlights code according to function context.

- Code in the global scope is one color. Code in functions within the global
  scope is a different color, and code within such functions is another color,
  and so on.
- Identifiers retain the color of the scope in which they were declared.
- Comments are gray.

Lexical scope information at-a-glance can assist a programmer in understanding
the overall structure of a program. It can also help curb nasty bugs like name
shadowing or unexpected assignment. A rainbow can indicate excessive
complexity. A spot of contrast followed by an assignment expression could be a
side-effect... or, the state of a closure could be undergoing change.

This coloring strategy is probably more useful than conventional *syntax*
highlighting. Highlighting keywords can help one to detect spelling errors, and
highlighting the content between quotation marks can alert one to unclosed
string literals. But a [linter][] could also spot those errors, and if
[integrated via flycheck][integration], an extra spot opens up in your editing
toolbelt.

Give context coloring a try; you may find that it *changes the way you write
code*.

## Features

Currently, only JavaScript language support has been implemented.

Extension is as simple as writing a "scopifier" for the language of your choice
and adding an entry to `context-coloring-scopifier-plist`. See
`languages/javascript/scopifier.js` for an example.

- Light and dark color schemes.
- Fast async AST parsing. Some total parse + recolor times:
  - jQuery (9191 lines): 0.63 seconds
  - Lodash (6786 lines): 0.37 seconds
  - Async (1124 lines): 0.17 seconds
  - mkdirp (98 lines): 0.09 seconds

## Usage

Requires Emacs 24+.

JavaScript language support requires [Node.js 0.10+][node].

- Clone this repository.

```bash
cd ~/.emacs.d/
git clone https://github.com/jacksonrayhamilton/context-coloring.git
```

- Add it to your [load path][].
- Add a mode hook for `context-coloring-mode`.

In your `~/.emacs` file:

```lisp
(add-to-list 'load-path "~/.emacs.d/context-coloring")
(require 'context-coloring)
(add-hook 'js-mode-hook 'context-coloring-mode)
```

[linter]: https://github.com/jacksonrayhamilton/jslinted
[integration]: https://github.com/jacksonrayhamilton/jslinted#emacs-integration
[node]: http://nodejs.org/download/
[load path]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Lisp-Libraries.html
