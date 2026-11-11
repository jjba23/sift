# How sift works

These are the moving parts a reader usually has to reconstruct from the source:
where a name is resolved, why a second diagnostic report arrives after the
first, how the standard library is found, and what a file rename changes.  The
diagrams are the map; the Org pages are the reference.

## Navigation: from a cursor to a definition

A jump is answered from the narrowest thing that knows the answer, and it only
widens when it has to.  The buffer is the authority, the project is indexed
beside it, and the library is the last and largest place a name can live.

```mermaid
flowchart TD
    A[Cursor position] --> B[Reader: AST with source positions]
    B --> C{In scope here?}
    C -- yes --> D[Parameter, let binding, internal definition]
    C -- no --> E{Defined in this buffer?}
    E -- yes --> F[Top-level definition]
    E -- no --> G{Open buffers define it?}
    G -- yes --> H[Other buffer]
    G -- no --> I{Project index defines it?}
    I -- yes --> J[File on disk]
    I -- no --> K{Standard library or downloaded source?}
    K -- yes --> L[Installed, compressed, or fetched source]
    K -- no --> M[No answer]
```

Why this is good: a jump almost never scans the project, because resolution
stops at the first place a reader would look.  The project index is read once
per session and refreshed on save or a watched file change; the standard
library is read once per installation and cached.

## Diagnostics: the fast answer, then the whole-file answer

Some rules can be answered while the reader is typing; others need the whole
file, and the compiler needs a process.  The editor gets the cheap answer
immediately, and each slower answer replaces it only for the text it was
computed from.

```mermaid
sequenceDiagram
    participant E as Editor
    participant S as sift
    participant D as Deep-pass thread
    participant C as Compiler thread
    E->>S: didOpen / didChange (full text)
    S->>S: fast rules
    S-->>E: publishDiagnostics (fast)
    S->>D: all warnings (in a thread)
    D-->>E: publishDiagnostics (fast + whole-file)
    E->>S: didOpen / didSave
    S->>C: compile a temporary copy
    C->>C: guild / emacs / sbcl, bounded output
    C-->>E: publishDiagnostics (all + compiler)
    Note over C: cached by text + dialect
```

Why this is good: a keystroke never waits for a compiler, a second report is
sent only when it says something new, and a report about text the editor no
longer has is dropped instead of drawn over the wrong buffer.

## Standard library: discovery, compression and the cache

The library is the largest code base a jump can enter and the one the project
does not contain.  It is indexed from the implementation's own source, the
package trees a dialect installs beside it, and optionally a source archive
named by URL.

```mermaid
flowchart LR
    A[stdlib setting] --> B{true / false / list}
    B -- true --> C[Ask the implementation for its roots]
    B -- list --> D[Local directories]
    B -- list --> E[Source URLs]
    C --> F[Roots]
    D --> F
    E --> G{download-sources?}
    G -- false --> H[Report: not fetched]
    G -- true --> I[curl into the cache]
    I --> J[tar listing checked]
    J --> K[Unpack under the cache]
    K --> F
    F --> L[Walk files]
    L --> M{.el.gz?}
    M -- yes --> N[gzip -dc while reading]
    M -- no --> O[Read as it is]
    N --> P[Definitions and names]
    O --> P
    P --> Q[(Index cache)]
```

Why this is good: the first index is expensive and the second session is a
cache read.  A compressed source is read through the decompressor the platform
already has, a URL is fetched only when the project asked for network access,
and an archive is listed and refused before it is extracted if a member would
leave the extraction directory.

## Organize imports: remove, then sort, in place

The two operations are independent, are on by default, and are the same
question in the editor and on the command line.

```mermaid
flowchart TD
    A[Buffer or file] --> B[unused-import fixes]
    B --> C[Remove unused specifications]
    C --> D[Sort import specifications by library]
    D --> E{Shape?}
    E -- "(import ...)" --> F[Permute specs in place]
    E -- "define-module" --> G[Permute #:use-module clauses in place]
    F --> H[One WorkspaceEdit or one written file]
    G --> H
```

Why this is good: sorting is a permutation of the existing spans, not a
pretty-print, so a one-line import form stays one line and a comment between
clauses stays where it is.  `optimize-imports` and `sort-imports` can be turned
off independently, and the standard `source.organizeImports` action respects
both.

## Rename a file: the module follows the name

A file rename asks the dialect what a module is.  Scheme and Emacs Lisp have an
answer; Common Lisp does not pretend a package is a file.

```mermaid
flowchart TD
    A[workspace/willRenameFiles] --> B{Dialect}
    B -- Scheme --> C[Find define-module / library name]
    C --> D{Old last segment equals old file name?}
    D -- no --> E[No edit: do not guess]
    D -- yes --> F[Replace last segment in the new file]
    F --> G[Update every import of the old library]
    B -- Emacs Lisp --> H[Find provide or old file name]
    H --> I[Update provide in the new file]
    I --> J[Update every require of the old feature]
    B -- Common Lisp --> K[No edit]
    G --> L[WorkspaceEdit]
    J --> L
    K --> L
    L --> M[workspace/didRenameFiles]
    M --> N[Forget old path, read new path, re-check buffers]
```

Why this is good: the rename is conservative.  A module whose name has nothing
to do with its file is left alone, a package is not renamed because a file
moved, and the index is corrected when the move actually happens rather than
guessed before it.

## Cross-platform notes

sift is written for POSIX systems, with Linux and macOS as the supported
targets.  The differences are in where a cache lives and which optional tools
are present, not in the library's behaviour.

| Concern | Linux | macOS |
|---|---|---|
| Cache | `$XDG_CACHE_HOME/sift`, else `~/.cache/sift` | `$XDG_CACHE_HOME/sift`, else `~/Library/Caches/sift` |
| Compressed source | `gzip` | `gzip` |
| Source download | `curl`, `tar` | `curl`, `tar` (bsdtar) |
| Compiler second opinion | `guild`, `emacs`, `sbcl` | `guild`, `emacs`, `sbcl` |
| Process capture | POSIX `sh -c 'exec "$@" 2>&1'` | the same |

A missing optional tool is reported, not fatal: without `gzip` a compressed
source is not read, without `curl`/`tar` a URL source is not fetched, and
without a compiler the second opinion is simply absent.  The core formatter,
linter and navigation over uncompressed files need only Guile 3.0.

Back to the [[file:../README.org][README]].
