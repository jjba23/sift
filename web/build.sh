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
# One command is the point.  Hoot is not a library sift ships; it is the
# compiler that turns sift's Scheme into WebAssembly, and it is needed once,
# here, and never at run time.  Finding it lives in web/toolchain.sh, which
# `web/check.sh' sources too: the two scripts need the same toolchain, and the
# difference between a build that works and one that half works is a question
# only that file knows how to ask.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/.." && pwd)
dist=${1:-"$here/dist"}

# ---------------------------------------------------------------- the toolchain

. "$here/toolchain.sh"

# ---------------------------------------------------------------- the build

# The stamp is how a browser is told that the sift it has is not the sift that
# is deployed.  Every asset here is served with a long `Cache-Control' by the
# nginx that serves jointhefreeworld.org, so a URL that stays the same stays in
# the browser's cache: the version, the commit and the minute together make a
# URL that changes when the build does, and editor.js carries it on to
# sift.wasm.  The `+' of a version suffix would be read as a space in a query
# string, so the separator is a dash.
version=$(sed -n 's/.*sift-version[[:space:]]*"\([0-9][0-9.]*\)".*/\1/p' \
    "$root/src/sift/records.scm" 2>/dev/null | head -n 1)
[ -n "$version" ] || version=unreleased
revision=$(git -C "$root" rev-parse --short HEAD 2>/dev/null || echo nogit)
stamp="$version-$revision-$(date -u +%Y%m%dT%H%M%SZ)"

note "compiling web/main.scm -> $dist/sift.wasm"
if [ -d "$dist" ] && [ "$dist" != "/" ]; then
    rm -rf "$dist"
fi
mkdir -p "$dist"

# The load path is the seam, in this order: web/compat first, so that an import
# the target does not have -- `(ice-9 format)', `(srfi srfi-13)' -- finds the
# stand-in rather than failing to resolve; then web/, which is where
# `(sift web)' lives; then src/, the core, which knows nothing about any of
# this.
#
# `-Ono-contify' because the `contify' pass of `-O2' miscompiles this program:
# the module compiles, and then `lint', `hover', `symbols' and `inlayHints' die
# with `WASM runtime error: invalid cast' -- a `ref.cast' of the fixnum 1 to a
# struct type, which is the pass sharing a continuation between branches that
# do not have the same type.  Every other pass stays on, so the module is the
# size and the speed it was; `web/probe.scm' and `maak web-check' are what say
# so, and they are what will say when the line can go.
"$guild" compile-wasm \
    -O2 \
    -Ono-contify \
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

# The page carries `__SIFT_BUILD__' where the stamp goes, in every URL it names.
# `editor.js' needs nothing substituted into it: it finds the highlighter and
# the wasm beside itself and carries its own query string on to them, which is
# the same arrangement `sift.wasm' already used and one placeholder fewer to
# forget.
sed -e "s|__SIFT_BUILD__|$stamp|g" "$here/index.html" > "$dist/index.html"
cp "$here/editor.js" "$dist/editor.js"
cp "$here/highlight.js" "$dist/highlight.js"
cp "$here/editor.css" "$dist/editor.css"

# The icon the project already has, at the size a tab asks for, so that a page
# with a favicon is a page whose server log has no 404 in it.
cp "$root/assets/icon/icon-128.png" "$dist/favicon.png"

# A directory that is missing one of these is a page that half works -- a 404
# on the wasm is a blank editor, and a 404 on reflect.js is a status bar that
# says "sift could not start" -- so the build says what it laid out, and fails
# when the answer is not everything.
for file in index.html editor.js highlight.js editor.css favicon.png sift.wasm reflect.js reflect.wasm wtf8.wasm; do
    [ -s "$dist/$file" ] || die "$dist/$file is missing: the build did not finish"
done

note "$dist is ready"
note "  build   $stamp"
for file in index.html editor.js highlight.js editor.css favicon.png sift.wasm reflect.js reflect.wasm wtf8.wasm; do
    printf 'sift:   %-14s %s\n' "$file" "$(du -h "$dist/$file" | cut -f 1)" >&2
done
note "serve it: cd $dist && python3 -m http.server 8080"
