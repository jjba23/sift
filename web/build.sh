#!/bin/sh
# sift --- build the browser build
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# Compiles web/main.scm to sift.wasm and lays out a directory that a static
# file server can serve: the editor page, the Wasm module, and the reflection
# runtime Hoot's loader needs beside it.  Nothing else is required -- no
# bundler, no package manager, no node_modules.
#
#   ./web/build.sh            # writes web/dist/
#   ./web/build.sh /tmp/out   # or wherever
#
# Hoot is not a library sift ships; it is the compiler that turns sift's Scheme
# into WebAssembly, and it is needed once, here, and never at run time.  The
# two things this script needs to find are `guild' (from Guile) and Hoot's own
# modules on the Guile load path, which is what `guix shell guile-hoot' or
# `nix shell' would set up.  `SIFT_HOOT_GUILE' points the script at a specific
# Guile when the one on PATH is not the one the toolchain was built for.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/.." && pwd)
dist=${1:-"$here/dist"}

guile=${SIFT_HOOT_GUILE:-guile}
guild=${SIFT_HOOT_GUILD:-guild}

if ! command -v "$guild" >/dev/null 2>&1; then
    echo "sift: $guild is not on PATH; Hoot provides it (see web/README.org)" >&2
    exit 1
fi

if ! "$guile" -c '(use-modules (hoot config))' >/dev/null 2>&1; then
    echo "sift: $guile cannot find (hoot config); set GUILE_LOAD_PATH to Hoot's"
    echo "      site directory, or SIFT_HOOT_GUILE to the Guile Hoot was built for" >&2
    exit 1
fi

echo "sift: compiling web/main.scm -> $dist/sift.wasm"
mkdir -p "$dist"

# The load path is the seam, in this order: web/compat first, so that a
# `(ice-9 ...)' import the target does not have finds the shim rather than
# failing to resolve; then web/, which is where `(sift web)' lives; then src/,
# the core, which knows nothing about any of this.
"$guild" compile-wasm \
    --bundle \
    -L "$root/src" \
    -L "$here" \
    -L "$here/compat" \
    -o "$dist/sift.wasm" \
    "$here/main.scm"

# `--bundle' has already copied the helper modules the reflection runtime
# loads (reflect.wasm and wtf8.wasm) next to the output; `reflect.js' is the
# half that stays JavaScript, and it ships with Hoot, so it is asked for rather
# than assumed.
reflect_js=$("$guile" -c '(use-modules (hoot config)) (display %reflect-js-dir)')
cp "$reflect_js/reflect.js" "$dist/reflect.js"

cp "$here/index.html" "$dist/index.html"
cp "$here/editor.js" "$dist/editor.js"
cp "$here/editor.css" "$dist/editor.css"

cp "$here/index.html" "$dist/index.html"
cp "$here/editor.js" "$dist/editor.js"

echo "sift: $dist is ready"
echo "sift: serve it, for example: cd $dist && python3 -m http.server 8080"
