# guts.el — Expression anatomy for Emacs Lisp

`guts` answers the question "what does this expression actually use?" It walks the Emacs Lisp form at point (or the active region) and reports every symbol it touches, sorted into categories: reader syntax, special forms, macros, functions, variables, and custom variables. Within each category it separates names defined in the current buffer from names defined elsewhere.

It is a reading aid, not a static analyzer: it works on the text, so it does not
need the code to have been evaluated.

## Installation

Put `guts.el` on your `load-path` and:

```elisp
(require 'guts)
```

No external dependencies; `seq` is required (built in since Emacs 25).

## Usage

`M-x guts-analyze-expression` reports on the enclosing expression at point.
If the region is active, only that region is analyzed.

Output goes to a `*guts sexp analysis*` buffer:

```
Reader syntax (1)
  `

Special forms (2)
  if interactive

Macros (4)
  local:    my-define-thing
  external: ` pcase-let* unless

Functions (13)
  local:    guts--expression-bounds guts-analyze-string
  external: buffer-substring-no-properties car cdr list princ

Variables (3)
  local:    guts--calls
  external: beg end text

Custom variables (0)
  local:    (none)
  external: (none)
```

## Categories

- **Reader syntax** — `` ` ``, `\,` and `\,@`. These are read-time markers with no
  binding of their own, so they are reported separately rather than as calls.
- **Special forms** — `if`, `while`, `let`, `setq`, …
- **Macros** — anything for which `macrop` returns non-nil.
- **Functions** — ordinary functions: `fboundp` but neither macro nor special form.
- **Variables** — free symbols that are `boundp`.
- **Custom variables** — a subset of variables for which `custom-variable-p`
  returns non-nil, i.e. symbols declared with `defcustom`.

`local:` names are those defined by a top-level form in the current buffer;
`external:` names are everything else. The split is omitted for Reader syntax and
Special forms, which cannot be defined in a buffer.

## How it reads code

The collector deliberately does *not* treat every symbol it sees as a call:

- Quoted data is skipped entirely — `'(foo bar)` yields nothing.
- `(function foo)` counts `foo` as a called symbol.
- Lambda arglists, `let`/`let*` binding names, and the names in `defvar`,
  `defcustom`, `defun`, `defmacro` and friends are treated as definitions
  rather than uses; their *values* and *bodies* are walked normally.
- Backquote templates are walked as data. Symbols inside `` `(foo bar) `` are
  not calls, but the payloads of `\,` and `\,@` are, since those are evaluated.

## Customization

- `guts--definition-heads` — form heads whose second element names something
  defined by that form. Extend this if your code uses other defining macros
  (`cl-defun`, `define-minor-mode` and similar are already included).
- `guts--special-forms-fallback` — used only when `special-form-p` is
  unavailable.

## Testing

The test suite uses ERT and assumes `guts.el` is loaded:

```
emacs -batch -l guts -l guts-tests -f ert-run-tests-batch-and-exit
```

or interactively, `M-x ert-run-tests-interactively`.

## Known limitations

- **Unevaluated definitions are invisible in the variables sections.**
  Variables are filtered with `boundp`, and customs with `custom-variable-p`,
  so a `defvar` or `defcustom` in the buffer that you have not yet evaluated is
  dropped before it can be reported as local.
- `(function (lambda ...))` is not descended into, so the body of an explicitly
  quoted lambda is skipped.
- `local:` detection only scans top-level forms; definitions nested inside
  `progn` or other wrappers are not seen.

## Author and license

```
author: Kyle Andrews
license: GPL-3.0-or-later
```
