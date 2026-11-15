# sift --- find a Guile and a guild that can compile WebAssembly
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# Sourced by web/build.sh and web/check.sh, which need the same toolchain for
# the same reason: Hoot is not a library sift ships, it is the compiler that
# turns sift's Scheme into WebAssembly, and it is needed at build time and
# nowhere else.  Finding it is the only hard part, and it is hard in one
# specific way worth writing down:
#
#   Hoot's compiled modules are bytecode, and bytecode belongs to the Guile
#   that compiled it.  `guix shell guile guile-hoot' puts Guile 3.0.9 beside a
#   Hoot built for Guile 3.0.10, and the failure is not "Hoot is missing" --
#   it is `Unbound variable: default-debug-level', from inside Hoot, half way
#   through a compile.  A toolchain is therefore not "a guile and a guild on
#   PATH"; it is a guile and a guild that can compile this one line of Scheme.
#
# So the question asked here is that one (`compiles' below), and when the
# answer is no the script asks Guix for a toolchain that can.  The three
# variables are the escapes for a machine Guix is not on:
#
#   SIFT_HOOT_GUILE, SIFT_HOOT_GUILD   the toolchain to use, trusted as given
#   SIFT_HOOT_SHELL                    the Guix packages to ask for, default
#                                      "guile-next guile-hoot"
#
# and `SIFT_BUILD_IN_SHELL' is the marker, so that the copy Guix starts does
# not ask Guix again.
#
# Sourcing this sets $guile, $guild, $shell_packages and $note/$die, or exits
# with a sentence about what to install.

guile=${SIFT_HOOT_GUILE:-guile}
guild=${SIFT_HOOT_GUILD:-guild}
shell_packages=${SIFT_HOOT_SHELL:-"guile-next guile-hoot"}

note() { echo "sift: $*" >&2; }
die() { echo "sift: $*" >&2; exit 1; }

# The one question that matters: can this pair compile a program?  A Hoot that
# answers `#f' is a Hoot whose bytecode belongs to another Guile, or a Hoot
# that is not there at all, and neither is something to build with.  The probe
# is a whole program of two lines, because a compile that gets as far as
# writing a file is a compile that works, and a compile that does not is the
# failure this file is here to avoid.
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
    # knows better than this file does which Guile it was built for.
    note "using the toolchain named by the environment: $guile, $guild"
elif compiles; then
    note "using $("$guile" --version | head -n 1) from PATH"
elif command -v guix >/dev/null 2>&1 && [ -z "${SIFT_BUILD_IN_SHELL:-}" ]; then
    note "the Hoot on PATH cannot compile; asking Guix for $shell_packages"
    note "(this is the first run on a machine; it takes a while and only once)"
    exec guix shell $shell_packages -- env SIFT_BUILD_IN_SHELL=1 "$0" "$@"
elif [ -n "${SIFT_BUILD_IN_SHELL:-}" ]; then
    echo "sift: $shell_packages did not give a working Hoot; it said:" >&2
    sed 's/^/sift:   /' "$probe_log" >&2
    die "set SIFT_HOOT_GUILE and SIFT_HOOT_GUILD to a Guile and guild built for it"
else
    echo "sift: no working Hoot, and no Guix to ask for one." >&2
    echo "sift: Hoot is the compiler that makes sift.wasm; it is needed here and" >&2
    echo "sift: nowhere else.  Install it and put its Guile on PATH:" >&2
    echo "sift:   guix shell guile-next guile-hoot -- $0" >&2
    echo "sift: or name a toolchain you already have:" >&2
    echo "sift:   SIFT_HOOT_GUILE=... SIFT_HOOT_GUILD=... $0" >&2
    die "see web/README.org, 'Building it'"
fi
