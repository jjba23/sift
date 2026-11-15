<!--
  sift --- a dialect-aware formatter and linter for Scheme, Racket, Emacs Lisp
  and Common Lisp.

  SPDX-License-Identifier: MIT
  Copyright © Josep Bigorra <jjbigorra@gmail.com>
-->

# Contributing to sift

sift reads a Lisp file, reports what is ill-formed in it and rewrites its
whitespace, without losing a byte: every comment, blank line and reader prefix
comes back exactly as it was read.  It is one Scheme program — a formatter, a
linter and a language server — under the MIT license, and it has no build step.

The long form of this page is [`doc/contributing.org`](doc/contributing.org):
the five steps a rule takes, the ten a dialect takes, and what the suite
checks of each.  This page is the front door.

## Try it, then run the suite

```shell
git clone https://github.com/jjba23/sift.git
cd sift
./bin/sift version          # the program is the checkout
maak test                   # three minutes; it should be green before you change anything
```

There is nothing to build — sift is Guile source and a POSIX shell launcher —
so a checkout is an installation, and the first test run is also the first
compile.

## Where things are

| Path | What it holds |
|---|---|
| `src/sift/reader.scm` | the permissive, location-preserving reader |
| `src/sift/format.scm` | the formatter and each dialect's indentation table |
| `src/sift/checkers/` | the rules, one family per module |
| `src/sift/lint.scm` | the lint driver: run, suppress, filter, fix |
| `src/sift/lsp.scm`, `analysis.scm` | the language server and the buffer questions |
| `src/sift/cli.scm` | arguments, dispatch, exit codes |
| `tests/checkers/` | one suite per rule |
| `tests/reader-*.scm`, `format-*.scm`, `lint-*.scm` | one suite per dialect per job |
| `doc/` | the manual: style, rules (generated), formatting, CLI, LSP, design |
| `web/` | sift compiled to WebAssembly, and the editor page around it |
| `emacs/`, `editors/` | the Emacs package, the Neovim/Vim and VS Code integrations |

## Adding a rule

The short version; [`doc/contributing.org`](doc/contributing.org) has the
reasons and a worked example.

1. Take the next free code in `rule-codes` in `src/sift/records.scm` — codes
   are never reused, so a new rule goes at the end with the next number.
2. Write the walker and its `(checker 'name "sentence" 'dialects proc …)`
   record in the family module under `src/sift/checkers/`, and add it to that
   module's list.
3. Add `tests/checkers/<rule-name>.scm`: a passing case, a failing case at a
   known line and column, a case per dialect the rule claims, and — if it has a
   fix — the fixed text plus a re-lint that must find nothing.  Iterate with
   `maak test-suite tests/checkers/<rule-name>.scm`.
4. Run `maak docs` to regenerate `doc/rules.org`.
5. State the new count where the project states it: the README,
   `doc/linting.org`, `web/README.org` and `VISION.md`.

`tests/catalogue.scm` fails, by name, when one of those is missing — including
a rule that no test mentions.

## Adding a dialect

Also short; the long version names every table and every place the reader has
to learn something.

1. `dialects` and `string->dialect` in `src/sift/records.scm` — the name and the
   aliases an editor sends.
2. `dialect-extensions` in `src/sift/reader.scm` — at least one extension.
3. The reader: `dialect-delimiters`, `dialect-brackets`, `dialect-name`, and
   the dialect-specific syntax.
4. `dialect-indent-tables` in `src/sift/format.scm`, and the defaults if they
   differ.
5. Each rule's dialect list — `'all` is the four that exist today, so a new
   dialect is included only where you say so.
6. `src/sift/stdlib.scm`, the CLI's sentence, the editor integrations, the
   browser's selector, and the reader/format/lint suites of its own.

`tests/dialects.scm` fails, by dialect and by name, for the mechanical half.

## Bug reports

The informative report is the input, the output you expected, and the first
section of `sift doctor` — the dialect it chose and the rules that ran usually
answer the question on their own.  A file sift refuses to parse is a bug even
when the file is broken: the message is obliged to say where.

A question that the documentation does not answer is a defect in the
documentation, and reporting it is the most useful bug report there is.

## Commits and pull requests

- `type(scope): summary`, imperative, lower case, no full stop, about seventy
  characters.  Types in use: `feat`, `fix`, `perf`, `refactor`, `test`, `docs`,
  `build`, `ci`, `chore`.
- The subject says what changed; the body says why and what was rejected.
- One logical change per commit, so either half can be reverted alone.
- Run the gates before committing — `maak test`, `maak check`,
  `maak self-check`, `maak fixture-check`, and `maak emacs-test` if the package
  was touched.  A red suite that is committed is stated in the message.

Pull requests on GitHub, or a patch by `git format-patch` and `git send-email`,
which is how the author usually works.  The review concerns the code.

## Terms

AI agents are welcome and [`AI_POLICY.md`](AI_POLICY.md) says on what terms: a
human is the author of record, tests come first, and nothing is merged or
released on a red suite.  Contributions are under the MIT license, and
[`LICENSE`](LICENSE) carries the text.
