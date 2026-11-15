#!/bin/sh
# sift --- check that the browser build runs, not only that it compiles
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# `web/build.sh' answers "does this compile"; this answers "does it work".
# The difference is not academic: a module miscompiled by an optimization pass
# compiles, loads, and then dies on the first request that reaches the bad
# code -- which is how `lint', `hover', `symbols' and `inlayHints' came to
# answer `WASM runtime error: invalid cast' in a tab while the build gate was
# green.
#
# So this compiles web/probe.scm -- which asks the facade every operation the
# protocol has -- with the same compiler and the same flags as the build, runs
# it in Hoot's own virtual machine (no browser, no JavaScript engine), and
# fails unless it says `ok'.
#
#   ./web/check.sh
#
# The toolchain is the one web/toolchain.sh finds, which is the one the build
# uses.  Nothing here writes to web/dist: a check is not a build.

set -eu

here=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
root=$(CDPATH= cd -- "$here/.." && pwd)

# ---------------------------------------------------------------- the toolchain

. "$here/toolchain.sh"

# ---------------------------------------------------------------- the check

# The flags are the build's flags.  A check that compiled with something else
# would answer a question about a module nobody ships -- and the optimization
# level is exactly what went wrong once.
work=$(mktemp -d -t sift-web-check.XXXXXX)
trap 'rm -rf "$work" "$probe_log" "$probe_dir"' EXIT INT TERM

note "compiling web/probe.scm and running it"
"$guild" compile-wasm \
    -O2 \
    -Ono-contify \
    --run \
    -L "$root/src" \
    -L "$here" \
    -L "$here/compat" \
    -o "$work/probe.wasm" \
    "$here/probe.scm" > "$work/out" 2> "$work/err" || true

# The verdict is the program's value, which the runner prints on a line of its
# own; the compiler's warnings are on standard error, where they cannot be
# mistaken for it.
verdict=$(tail -n 1 "$work/out")

case $verdict in
    '"ok '*) note "the browser build runs: $verdict" ;;
    *)
        echo "sift: the browser build did not run; the probe said:" >&2
        if [ -n "$verdict" ]; then
            echo "sift:   $verdict" >&2
        fi
        echo "sift: and the compiler and runtime said:" >&2
        grep -v '^WARNING' "$work/err" | tail -n 8 | sed 's/^/sift:   /' >&2
        echo "sift: web/README.org, 'Checking that it runs', says what to look at." >&2
        exit 1
        ;;
esac
