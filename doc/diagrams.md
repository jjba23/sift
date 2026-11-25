# How sift works

These are the moving parts a reader usually has to reconstruct from the source:
where a name is resolved, why a second diagnostic report arrives after the
first, how the standard library is found, and what a file rename changes.  The
diagrams are the map; the Org pages are the reference.

The palette is a terminal: green for what every run does, cyan for the project
around the buffer, amber for what the reader or a note has to say, violet for
the places a name can live outside the project, and red for the answer that is
absent or thrown away.  Each diagram is drawn on its own canvas, so it reads the
same whatever theme the viewer uses.

## Navigation: from a cursor to a definition

A jump is answered from the narrowest thing that knows the answer, and it only
widens when it has to.  The buffer is the authority, the project is indexed
beside it, and the library is the last and largest place a name can live.

```mermaid
%%{init: {"theme": "base", "themeVariables": {"darkMode": true, "fontFamily": "monospace", "fontSize": "13px", "background": "#05070a", "mainBkg": "#05070a", "primaryColor": "#05070a", "primaryTextColor": "#39ff14", "primaryBorderColor": "#39ff14", "secondaryColor": "#0b0f14", "tertiaryColor": "#0b0f14", "lineColor": "#39ff14", "textColor": "#c9ffd4", "edgeLabelBackground": "#05070a", "clusterBkg": "#0b0f14", "clusterBorder": "#00e5ff"}, "flowchart": {"wrappingWidth": 340, "nodeSpacing": 16, "rankSpacing": 24}}}%%
flowchart LR
    classDef ttyCore fill:#001a0f,stroke:#39ff14,stroke-width:2px,color:#39ff14
    classDef ttyIndex fill:#00121a,stroke:#00e5ff,stroke-width:2px,color:#7df9ff
    classDef ttyLib fill:#100020,stroke:#b967ff,stroke-width:2px,color:#d9a6ff
    classDef ttyBad fill:#1a0005,stroke:#ff3860,stroke-width:2px,color:#ff9aa9

    subgraph canvas ["[ sift :: navigation · narrowest first ]"]
        A(["> cursor"])
        C["1 · in scope<br/>let, define"]
        E["2 · this buffer<br/>top-level"]
        G["3 · open buffers<br/>another buffer"]
        I["4 · project index<br/>file on disk"]
        K["5 · stdlib<br/>or fetched"]
        M["6 · no answer"]

        A --> C
        C --> E
        E --> G
        G --> I
        I --> K
        K --> M
    end

    class A,C,E ttyCore
    class G,I ttyIndex
    class K ttyLib
    class M ttyBad
    style canvas fill:#05070a,stroke:#39ff14,stroke-width:2px,color:#39ff14
    linkStyle default stroke:#39ff14,stroke-width:1.5px
```

Each rung is asked only when the one before it had no answer, so the arrow is
"not found here" and never "also look here".

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
%%{init: {"theme": "base", "themeVariables": {"darkMode": true, "fontFamily": "monospace", "fontSize": "13px", "background": "#05070a", "mainBkg": "#05070a", "primaryColor": "#05070a", "primaryTextColor": "#39ff14", "primaryBorderColor": "#39ff14", "secondaryColor": "#0b0f14", "tertiaryColor": "#0b0f14", "lineColor": "#39ff14", "textColor": "#c9ffd4", "edgeLabelBackground": "#05070a", "clusterBkg": "#0b0f14", "clusterBorder": "#00e5ff", "actorBkg": "#0b0f14", "actorBorder": "#00e5ff", "actorTextColor": "#7df9ff", "actorLineColor": "#00e5ff", "signalColor": "#39ff14", "signalTextColor": "#c9ffd4", "labelBoxBkgColor": "#0b0f14", "labelBoxBorderColor": "#39ff14", "labelTextColor": "#39ff14", "loopTextColor": "#c9ffd4", "noteBkgColor": "#1a1200", "noteBorderColor": "#ffb000", "noteTextColor": "#ffd27d", "activationBkgColor": "#001a0f", "activationBorderColor": "#39ff14"}}}%%
sequenceDiagram
    participant E as Editor
    participant S as sift
    participant D as Deep pass
    participant C as Compiler
    E->>S: didOpen / didChange
    S->>S: fast rules
    S-->>E: diagnostics (fast)
    S->>D: warnings, in a thread
    D-->>E: diagnostics (fast + full)
    E->>S: didSave
    S->>C: compile a temp copy
    C->>C: guild / emacs / sbcl
    C-->>E: diagnostics (all + compiler)
    Note over C: cached by text and dialect
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
%%{init: {"theme": "base", "themeVariables": {"darkMode": true, "fontFamily": "monospace", "fontSize": "13px", "background": "#05070a", "mainBkg": "#05070a", "primaryColor": "#05070a", "primaryTextColor": "#39ff14", "primaryBorderColor": "#39ff14", "secondaryColor": "#0b0f14", "tertiaryColor": "#0b0f14", "lineColor": "#39ff14", "textColor": "#c9ffd4", "edgeLabelBackground": "#05070a", "clusterBkg": "#0b0f14", "clusterBorder": "#00e5ff"}, "flowchart": {"wrappingWidth": 340, "nodeSpacing": 16, "rankSpacing": 24}}}%%
flowchart TD
    classDef ttyCore fill:#001a0f,stroke:#39ff14,stroke-width:2px,color:#39ff14
    classDef ttyWarn fill:#1a1200,stroke:#ffb000,stroke-width:2px,color:#ffd27d
    classDef ttyIndex fill:#00121a,stroke:#00e5ff,stroke-width:2px,color:#7df9ff

    subgraph canvas ["[ sift :: stdlib ]"]
        A(["> stdlib setting"])
        C["implementation roots"]
        D["local directories"]
        E["source URLs"]
        F["roots"]
        G{"download-sources?"}
        H["reported, not fetched"]
        I["fetch, list, then unpack"]
        L["walk files"]
        M{".el.gz?"}
        N["gzip -dc on read"]
        O["read as it is"]
        P["definitions and names"]
        Q[("index cache")]

        A -->|true| C
        A -->|list| D
        A -->|list| E
        C --> F
        D --> F
        E --> G
        G -->|no| H
        G -->|yes| I
        I --> F
        F --> L
        L --> M
        M -->|yes| N
        M -->|no| O
        N --> P
        O --> P
        P --> Q
    end

    class A,C,D,E,F,L,M,N,O,P ttyCore
    class G,H ttyWarn
    class I,Q ttyIndex
    style canvas fill:#05070a,stroke:#39ff14,stroke-width:2px,color:#39ff14
    linkStyle default stroke:#39ff14,stroke-width:1.5px
    linkStyle 7 stroke:#ffb000,stroke-width:1.5px
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
%%{init: {"theme": "base", "themeVariables": {"darkMode": true, "fontFamily": "monospace", "fontSize": "13px", "background": "#05070a", "mainBkg": "#05070a", "primaryColor": "#05070a", "primaryTextColor": "#39ff14", "primaryBorderColor": "#39ff14", "secondaryColor": "#0b0f14", "tertiaryColor": "#0b0f14", "lineColor": "#39ff14", "textColor": "#c9ffd4", "edgeLabelBackground": "#05070a", "clusterBkg": "#0b0f14", "clusterBorder": "#00e5ff"}, "flowchart": {"wrappingWidth": 340, "nodeSpacing": 16, "rankSpacing": 24}}}%%
flowchart TD
    classDef ttyCore fill:#001a0f,stroke:#39ff14,stroke-width:2px,color:#39ff14
    classDef ttyIndex fill:#00121a,stroke:#00e5ff,stroke-width:2px,color:#7df9ff

    subgraph canvas ["[ sift :: imports ]"]
        A(["> buffer or file"])
        B["unused-import fixes"]
        C["remove unused specifications"]
        D["sort specifications by library"]
        E{"shape?"}
        F["permute specs in place"]
        G["permute #:use-module in place"]
        H["one WorkspaceEdit, one written file"]

        A --> B
        B --> C
        C --> D
        D --> E
        E -->|"(import ...)"| F
        E -->|"define-module"| G
        F --> H
        G --> H
    end

    class A,B,C,D,E ttyCore
    class F,G,H ttyIndex
    style canvas fill:#05070a,stroke:#39ff14,stroke-width:2px,color:#39ff14
    linkStyle default stroke:#39ff14,stroke-width:1.5px
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
%%{init: {"theme": "base", "themeVariables": {"darkMode": true, "fontFamily": "monospace", "fontSize": "13px", "background": "#05070a", "mainBkg": "#05070a", "primaryColor": "#05070a", "primaryTextColor": "#39ff14", "primaryBorderColor": "#39ff14", "secondaryColor": "#0b0f14", "tertiaryColor": "#0b0f14", "lineColor": "#39ff14", "textColor": "#c9ffd4", "edgeLabelBackground": "#05070a", "clusterBkg": "#0b0f14", "clusterBorder": "#00e5ff"}, "flowchart": {"wrappingWidth": 340, "nodeSpacing": 16, "rankSpacing": 24}}}%%
flowchart TD
    classDef ttyCore fill:#001a0f,stroke:#39ff14,stroke-width:2px,color:#39ff14
    classDef ttyIndex fill:#00121a,stroke:#00e5ff,stroke-width:2px,color:#7df9ff
    classDef ttyWarn fill:#1a1200,stroke:#ffb000,stroke-width:2px,color:#ffd27d

    subgraph canvas ["[ sift :: rename ]"]
        A(["workspace/willRenameFiles"])
        B{"dialect"}
        C["find define-module"]
        D{"segment is the file name?"}
        E["no edit"]
        F["replace the last segment"]
        G["update imports"]
        H["find provide"]
        I["update provide"]
        J["update requires"]
        K["no edit"]
        L["WorkspaceEdit"]
        M(["workspace/didRenameFiles"])
        N["forget old path, re-check buffers"]

        A --> B
        B -->|Scheme| C
        C --> D
        D -->|no| E
        D -->|yes| F
        F --> G
        B -->|Emacs Lisp| H
        H --> I
        I --> J
        B -->|Common Lisp| K
        G --> L
        J --> L
        K --> L
        L --> M
        M --> N
    end

    class A,B,C,D,F,H,I,L,M,N ttyCore
    class G,J ttyIndex
    class E,K ttyWarn
    style canvas fill:#05070a,stroke:#39ff14,stroke-width:2px,color:#39ff14
    linkStyle default stroke:#39ff14,stroke-width:1.5px
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
