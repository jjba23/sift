# Vision

sift reads a Lisp source file as a tree of forms that carry source positions,
reports what is ill-formed in it, and prints the tree back with the whitespace
moved. Formatting is a total function on text and a fixed point, and every
fixture in the suite asserts both. This document says what that premise should
eventually buy — in four dialects, five editors, a pipeline and a browser tab —
and which refusals are worth keeping.

## Where sift is today

Twenty-one modules under `src/sift/`, about sixteen thousand lines of Guile 3
with no C dependency and no build step, MIT licensed. `reader.scm` is a reader
of its own, because `read` discards comments, keeps no positions and cannot
represent a block comment, a datum comment or a reader prefix as a node. The
rest follows: byte-exact preservation, one traversal with the answers kept, a
fix that replaces exactly one range, a diff whose hunks are the change.

What exists and is tested:

- **Four dialects, each by its own report.** A square bracket is a vector in
  Emacs Lisp and a `cond` clause in Scheme, `#{…}#` is a symbol only Guile
  reads, a `#lang` line is the header of a Racket module and an ambiguous file
  is never guessed at.
- **93 rules**, each a record with a name, a stable code (`SIFT012`), a
  sentence, its dialects, a severity and an example. `sift explain` reads the
  reasoning out of the program; `doc/rules.org` is generated from the records.
- **A linter whose fixes refuse to guess.** A fix replaces one range, and a
  whole run's fixes are discarded if together they would introduce a parse
  error. Suppression is a comment — `;; sift:disable-next-line
  keyword-arguments`, `disable`, `enable` — line-oriented and form-scoped,
  because an exception list in a configuration file is a list no review reaches.
- **Five output formats** — the compile-style line, JSON, SARIF 2.1.0, GitHub
  annotations and `null` — so a compilation buffer, a code-scanning service and
  a script reading the exit status are told the same thing.
- **A language server**, `sift lsp`: diagnostics, quickfixes, formatting,
  definition, references, an outline, completion, inlay hints, hover, signature
  help, rename, five code actions and import organization — and a
  `method not found` for everything else rather than a guess. The clients are
  thin wrappers over it: `emacs/sift.el` for Flymake and Eglot, a Neovim setup, a
  Vim plugin with a compiler, a VS Code extension, a pre-commit hook and two
  GitHub Actions.
- **A configuration file that is data**: `sift.yaml` or a trusted `sift.scm`,
  four layers from system to command line, an unknown key refused with its
  position, `sift config show --verbose` naming the layer that won.
- **A browser build**: `web/` compiles the same reader, formatter and rules with
  Guile Hoot, over a compatibility layer, with eight tests holding the dependency
  one way so the tab's answers are byte for byte the command line's.

What it does not do is documented as carefully. `doc/design.org` states six
propositions; `doc/language-server.org` is an exhaustive table of answered and
refused requests; `doc/style.org` ends with what sift will not do. Two limits
matter most: a reference is matched by name within one dialect, because sift
expands no macro and follows no import; and a project is read once per session
into an immutable snapshot rather than waited for.

## The vision

1. **The style becomes a specification, not an implementation.** Each rule with
   its rationale, and a conformance corpus — the fixtures already are one — so
   that a second implementation can be checked rather than merely read. The
   browser build proves one codebase can serve two surfaces; a specification
   would prove the style is more than this codebase's opinion.
2. **Resolution across files.** Following `define-module`, `#:use-module`,
   `defpackage` and `provide` far enough that a reference is a binding rather
   than a name, and a project-wide rename is correct in every file that uses the
   definition. That is the difference between a linter for a file and a tool for
   a project.
3. **Project-scale operations that stay reviewable.** A rename, a fix-all or a
   formatting sweep should produce one patch whose hunks are the change, with
   the guarantees sift already gives one file: no invented delimiters, no made-up
   names, no fix that cannot be re-linted clean.

## Features that are in scope and not built yet

- **Range formatting and format-on-type, and fold ranges.** Formatting a
  fragment is refused because the formatter indents from column zero, so it
  would land at the margin; the hard part is an origin column that still
  produces, for the range it touched, the bytes a whole-buffer run would.
  Folding is refused because the formatter has no opinion about it, and the
  structure is already in the tree — what is missing is the policy, which is a
  style decision to take rather than derive.
- **Macro-introduced names in references and rename**, and with them a call
  hierarchy, a declaration and an implementation. Named as limitations beside the
  request table. The hard part is unwinding an invocation without an expander,
  which is why the declared table above is the cheaper path.
- **Pull diagnostics (`textDocument/diagnostic`).** Refused; findings are pushed
  after every change. The hard part is holding one diagnosis in two models
  without the lint driver becoming two code paths that drift.
- **A Racket standard library.** The fourth dialect was added the way the entry
  here said it would have to be: the reader, the formatter and each rule
  re-argued, and the rules whose premise is another dialect's left out. What it
  does not have is a library index, because asking `racket/base` what it exports
  means shipping a Racket program sift would have to maintain the way it
  maintains the Guile, Emacs Lisp and SBCL ones; a Racket buffer is answered for
  by the buffer and the project alone.
- **Linting a change rather than a tree.** A pipeline wants findings on the lines
  a diff touched. The hard part is the failing decision: whether a job fails on a
  new finding only.

## New features that would be worth having

- **`sift new-rule`**: writes the record, allocates the next free code and
  creates the test file with the three cases the contributing guide asks for, so
  adding a rule is a command and a filled blank.
- **Range-limited analysis in the browser.** A 6 kB buffer costs 1.2 s to lint
  today; sending only the enclosing top-level form would put every editable
  buffer under the debounce, since a form is what a person is writing.
- **A `--since REV` mode** for `lint` and `ci`, with findings attributed to hunks
  and a severity policy for new versus existing ones — what a large codebase
  needs before it can adopt a linter at all; and an **`--explain`** on `sift
  fix` that names the rule and range behind each applied fix.
- **A complexity gate.** `etc/complexity.scm` already reports cognitive and
  cyclomatic complexity and nesting per definition with sift's own reader, and
  notes that a budget is only worth adding if one is wanted. Wiring it into `ci`
  would make refactoring measurable.

## Contributions that would move the needle

1. **A rule.** The path is written down and short: one record, one test file,
   three cases. It is the cheapest thing here that a stranger can land in an
   afternoon.
2. **A dialect detail.** An escape spelled differently, a comment in an
   unexpected place, a body that indents by something other than two:
   `tests/reader-*.scm` and `tests/format-*.scm` hold the behaviour, and each of
   those is a bug report that comes with its fix.
3. **An editor integration.** Neovim, Vim and VS Code each have a directory and a
   test in that editor's own tool; a fourth editor is one directory more, and it
   is how the project stops being an Emacs project that also has an LSP.
4. **A sentence in the documentation** that removes a hesitation. The README and
   `doc/style.org` are read by people deciding whether to trust the tool.
5. **Range formatting.** Medium-sized, touches the formatter's contract rather
   than its rules, and it is the missing piece for editor parity.
6. **The declared-macro table**, with a test per convention it enables: the
   largest correctness win available, and one that needs an argument first.
7. **A `--since` mode**, because it is what lets a large codebase adopt the
   linter without a mass reformat.

## Compared to related projects

| Project | What it is | Where sift differs |
| --- | --- | --- |
| cljfmt | A Clojure formatter that rewrites indentation and whitespace in place | Clojure only, and it reformats from a parsed form rather than preserving bytes; sift's fixed point is asserted over a fixture corpus, and it lints as well as formats |
| clj-kondo | A Clojure linter with a checkable macro configuration and an LSP server | Its macro configuration is exactly the gap the vision names; but it lints only, has no formatter, and its analysis is Clojure's |
| rustfmt, gofmt | The canonical formatters of Rust and Go, shipped with the toolchain | One language, one style, no linting; their authority comes from being canonical, which sift approaches by writing the style down rather than by fiat |
| clang-format | A configurable C-family formatter with named styles and per-directory configuration | Configuration is its product and a hundred options are the price; sift has few settings, a stated style, and refuses to reflow |
| Prettier | An opinionated formatter for many languages, with almost no options | The closest philosophy — one style, minimal knobs — but it reflows, and its guarantees are about output rather than about preserving every comment and blank line |
| Emacs's own toolchain | `indent.el`, `checkdoc`, the byte-compiler and Flymake: what Emacs Lisp has instead | No CLI, no CI shape, no JSON, no SARIF; the conventions are real and live in modes, which is why the Emacs Lisp rules here record what the byte-compiler reports |
| `scheme-mode` and `guild compile -W3` | The Scheme status quo: editor indentation and compiler warnings | Indentation in the editor, warnings from the compiler, nothing between them, and nothing a pipeline can read as one report |
| SLIME, SLY | Common Lisp environments with indentation specs, cross-references and a debugger | Their indentation specs are the idea a declared-macro table would adopt; but they are an environment, not a program, and their answers need a running image |

The others do three things sift should reach for: a macro configuration a
project can check in (clj-kondo), answers from a real analysis of the program
rather than from names (SLIME, SLY), and the authority that comes from being the
one canonical tool of a language (rustfmt, gofmt). None carries a browser build
of the same code, and none treats four dialects in one executable as the
premise.

Where sift is ahead, and should stay ahead: the fixed point, asserted over every
fixture rather than promised; a rule catalogue that is data first, so the
documentation, the diagnostics and the browser's rule list cannot drift from the
code; refusals documented request by request instead of approximated; the
browser being a compile of the same modules rather than a second formatter in
JavaScript; and one Scheme program with no build step, which is why
`guix install sift` gives a formatter and an LSP server on a machine that has
never compiled anything.

## In the long run

- **The style is the artefact.** If the corpus and the rationale are good
  enough, the implementation is replaceable and the conventions outlive it —
  the bet gofmt won, made in a much smaller community.
- **One analysis, four consumers.** Findings, fixes, hover and inlay hints are
  four renderings of one walk. Keeping them so is what lets a new output format
  or a new client be an afternoon's work.
- **A fifth dialect.** Racket was the fourth, and adding it cost what this
  document predicted: a table, a dozen rules re-argued, a corpus of its own.
  Each one after it is more evidence that the reader, the formatter and the rule
  records are the general part and the dialect the detail.

## Closing

The in-scope items are ordered by what a stranger can land first: a rule, a
dialect detail, an editor, a sentence. The vision items change the shape of the
analysis rather than its catalogue, and deserve an argument before a patch.
Everything above should be read against the six propositions in
`doc/design.org`.

*This document describes intent, not commitments.*
