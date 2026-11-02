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
# here, and never at run time.  Finding it is the only hard part of this
# script, and it is hard in one specific way worth writing down:
#
#   Hoot's compiled modules are bytecode, and bytecode belongs to the Guile
#   that compiled it.  `guix shell guile guile-hoot' puts Guile 3.0.9 beside a
#   Hoot built for Guile 3.0.10, and the failure is not "Hoot is missing" --
#   it is `Unbound variable: default-debug-level', from inside Hoot, half way
#   through a compile.  A toolchain is therefore not "a guile and a guild on
#   PATH"; it is a guile and a guild that can compile this one line of Scheme.
#
# So the script asks that question instead (`compiles' below), and when the
# answer is no it asks Guix for a toolchain that can.  The three variables are
# the escapes for a machine Guix is not on:
#
#   SIFT_HOOT_GUILE, SIFT_HOOT_GUILD   the toolchain to use, trusted as given
#   SIFT_HOOT_SHELL                    the Guix packages to ask for, default
#                                      "guile-next guile-hoot"
#
# and `SIFT_BUILD_IN_SHELL' is this script's own marker, so that the copy
# Guix starts does not ask Guix again.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/.." && pwd)
dist=${1:-"$here/dist"}

guile=${SIFT_HOOT_GUILE:-guile}
guild=${SIFT_HOOT_GUILD:-guild}
shell_packages=${SIFT_HOOT_SHELL:-"guile-next guile-hoot"}

note() { echo "sift: $*" >&2; }
die() { echo "sift: $*" >&2; exit 1; }

# ---------------------------------------------------------------- the toolchain

# The one question that matters: can this pair compile a program?  A Hoot that
# answers `#f' is a Hoot whose bytecode belongs to another Guile, or a Hoot
# that is not there at all, and neither is something to build with.  The probe
# is a whole program of two lines, because a compile that gets as far as
# writing a file is a compile that works, and a compile that does not is the
# failure this script is here to avoid.
probe_log=$(mktemp -t sift-hoot-probe.XXXXXX)
probe_dir=$(mktemp -d -t sift-hoot-probe.XXXXXX)
trap 'rm -rf "$probe_log" "$probe_dir"' EXIT INT TERM

compiles() {
    command -v "$guile" >/dev/null 2>&1 || return 1
    command -v "$guild" >/dev/null 2>&1 || return 1
    printf '(import (scheme base) (scheme write))\n(display "ok")\n' \
        > "$probe_dir/main.scm"
    rm -f "$probe_dir/probe.wasm"
    "$guild" compile-wasm -o "$probe_dir/probe.wasm" "$probe_dir/main.scm" \
        > "$probe_log" 2>&1 || return 1
    [ -s "$probe_dir/probe.wasm" ]
}

if [ -n "${SIFT_HOOT_GUILE:-}" ] || [ -n "${SIFT_HOOT_GUILD:-}" ]; then
    # Named by the caller, and trusted as named: a machine with its own Hoot
    # knows better than this script does which Guile it was built for.
    note "using the toolchain named by the environment: $guile, $guild"
elif compiles; then
    note "using $("$guile" --version | head -n 1) from PATH"
elif command -v guix >/dev/null 2>&1 && [ -z "${SIFT_BUILD_IN_SHELL:-}" ]; then
    note "the Hoot on PATH cannot compile; asking Guix for $shell_packages"
    note "(this is the first build on a machine; it takes a while and only once)"
    exec guix shell $shell_packages -- env SIFT_BUILD_IN_SHELL=1 "$0" "$@"
elif [ -n "${SIFT_BUILD_IN_SHELL:-}" ]; then
    echo "sift: $shell_packages did not give a working Hoot; it said:" >&2
    sed 's/^/sift:   /' "$probe_log" >&2
    die "set SIFT_HOOT_GUILE and SIFT_HOOT_GUILD to a Guile and guild built for it"
else
    echo "sift: no working Hoot, and no Guix to ask for one." >&2
    echo "sift: Hoot is the compiler that makes sift.wasm; it is needed here and" >&2
    echo "sift: nowhere else.  Install it and put its Guile on PATH:" >&2
    echo "sift:   guix shell guile-next guile-hoot -- ./web/build.sh" >&2
    echo "sift: or name a toolchain you already have:" >&2
    echo "sift:   SIFT_HOOT_GUILE=... SIFT_HOOT_GUILD=... ./web/build.sh" >&2
    die "see web/README.org, 'Building it'"
fi

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

# The page carries `__SIFT_BUILD__' where the stamp goes, in every URL it names.
# The other two are copied as they are: a stylesheet and a module that reads its
# own URL need nothing substituted into them.
sed -e "s|__SIFT_BUILD__|$stamp|g" "$here/index.html" > "$dist/index.html"
cp "$here/editor.js" "$dist/editor.js"
cp "$here/editor.css" "$dist/editor.css"

# The icon the project already has, at the size a tab asks for, so that a page
# with a favicon is a page whose server log has no 404 in it.
cp "$root/assets/icon/icon-128.png" "$dist/favicon.png"

# A directory that is missing one of these is a page that half works -- a 404
# on the wasm is a blank editor, and a 404 on reflect.js is a status bar that
# says "sift could not start" -- so the build says what it laid out, and fails
# when the answer is not everything.
for file in index.html editor.js editor.css favicon.png sift.wasm reflect.js reflect.wasm wtf8.wasm; do
    [ -s "$dist/$file" ] || die "$dist/$file is missing: the build did not finish"
done

note "$dist is ready"
note "  build   $stamp"
for file in index.html editor.js editor.css favicon.png sift.wasm reflect.js reflect.wasm wtf8.wasm; do
    printf 'sift:   %-14s %s\n' "$file" "$(du -h "$dist/$file" | cut -f 1)" >&2
done
note "serve it: cd $dist && python3 -m http.server 8080"
