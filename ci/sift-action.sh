#!/bin/sh
# sift --- the command behind the GitHub action
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# This file is the whole action.  `action.yml' is a wrapper that turns a step's
# inputs into the environment variables below, and this script is what runs:
# which is to say the action can be run, and tested, on a laptop.
#
#   SIFT_PATHS        the files or directories to judge           (default .)
#   SIFT_MODE         ci, lint or check                           (default ci)
#   SIFT_SEVERITY     error, warning or info                      (default warning)
#   SIFT_SARIF_FILE   where to write the SARIF document           (default sift.sarif)
#   SIFT_ANNOTATIONS  true to annotate the diff with `--format=github' (default true)
#   SIFT_INSTALL      auto, path, guix or nix                     (default auto)
#   SIFT_FAIL_ON_FINDINGS  false to report findings and pass      (default true)
#   SIFT_ACTION_DIR   the checkout of sift to build if it must be  (default its own)
#
# `auto' uses a `sift' already on the PATH -- what a Nix shell, a Guix
# environment or an earlier step provides -- and otherwise builds one from the
# checkout it lives in, with guix or with nix.  Every install mode answers with
# one executable, so the rest of this file does not know which one it is.
#
# The exit status is sift's: 0 when the tree is clean, 1 when sift found
# something, 2 when sift could not do what it was asked.  A step that treats 1
# as a failure is a step that fails the build on findings, which is what a
# gate is for.

set -eu

say() {
  printf '%s\n' "$*"
}

die() {
  printf 'sift action: %s\n' "$*" >&2
  exit 2
}

paths=${SIFT_PATHS:-.}
mode=${SIFT_MODE:-ci}
severity=${SIFT_SEVERITY:-warning}
sarif_file=${SIFT_SARIF_FILE:-sift.sarif}
annotations=${SIFT_ANNOTATIONS:-true}
install_mode=${SIFT_INSTALL:-auto}
fail_on_findings=${SIFT_FAIL_ON_FINDINGS:-true}
action_dir=${SIFT_ACTION_DIR:-$(cd "$(dirname "$0")/.." && pwd)}

SIFT=

build_with_guix() {
  store=$(guix build -f "$action_dir/guix.scm") ||
    die "guix could not build sift from $action_dir"
  SIFT=$store/bin/sift
}

build_with_nix() {
  [ -f "$action_dir/default.nix" ] ||
    die "no default.nix in $action_dir to build sift with"
  store=$(nix-build "$action_dir/default.nix" --no-out-link) ||
    die "nix could not build sift from $action_dir"
  SIFT=$store/bin/sift
}

resolve_sift() {
  case $install_mode in
    path)
      sift=$(command -v sift) ||
        die "install is path, but there is no sift on the PATH"
      SIFT=$sift
      ;;
    guix) build_with_guix ;;
    nix) build_with_nix ;;
    auto)
      if sift=$(command -v sift); then
        SIFT=$sift
      elif command -v guix >/dev/null 2>&1; then
        build_with_guix
      elif command -v nix-build >/dev/null 2>&1; then
        build_with_nix
      else
        die "no sift on the PATH, and neither guix nor nix to build one with"
      fi
      ;;
    *)
      die "unknown install '$install_mode'; sift knows auto, path, guix and nix"
      ;;
  esac
}

resolve_sift
say "sift action: $SIFT"
"$SIFT" version || die "$SIFT does not answer to version"

# --- annotations ------------------------------------------------------
#
# A finding annotated on the line of the diff is a finding a reviewer reads.
# The status is kept rather than raised: the SARIF pass below runs either way,
# so one step reports both what a reviewer sees and what code scanning keeps.

status=0
if [ "$annotations" = true ]; then
  # shellcheck disable=SC2086
  "$SIFT" "$mode" --format=github --severity="$severity" $paths || status=$?
fi

# --- the SARIF document -----------------------------------------------
#
# Written after the annotations so that a crash in the second pass cannot cost
# the first one's output.  A status of 2 means sift could not judge the tree, so
# the document is discarded rather than uploaded half-written.

findings=0
if [ -n "$sarif_file" ]; then
  directory=$(dirname "$sarif_file")
  [ "$directory" = . ] || mkdir -p "$directory"
  sarif_status=0
  # shellcheck disable=SC2086
  "$SIFT" "$mode" --format=sarif --severity="$severity" $paths >"$sarif_file" ||
    sarif_status=$?
  if [ "$sarif_status" = 2 ]; then
    rm -f "$sarif_file"
    status=2
  else
    findings=$(grep -o '"ruleId"' "$sarif_file" | wc -l | tr -d ' ')
    [ "$sarif_status" -gt "$status" ] && status=$sarif_status
  fi
fi

# --- what the run says in the job summary -----------------------------

if [ -n "${GITHUB_STEP_SUMMARY:-}" ]; then
  {
    printf '### sift\n\n'
    printf '| what | value |\n'
    printf '|---|---|\n'
    printf '| command | `sift %s --severity=%s %s` |\n' "$mode" "$severity" "$paths"
    printf '| findings | %s |\n' "$findings"
    printf '| sarif | `%s` |\n' "$sarif_file"
  } >>"$GITHUB_STEP_SUMMARY"
fi

if [ -n "${GITHUB_OUTPUT:-}" ]; then
  {
    printf 'sarif-file=%s\n' "$sarif_file"
    printf 'findings=%s\n' "$findings"
  } >>"$GITHUB_OUTPUT"
fi

if [ "$status" = 0 ]; then
  say "sift action: clean"
else
  say "sift action: $findings finding(s) in the SARIF document"
fi

# A gate is the default, because that is what a pipeline is for.  A caller that
# only wants the report says so, and a status of 2 -- sift could not do what it
# was asked -- always fails, because that is not a finding about the code.
if [ "$status" = 1 ] && [ "$fail_on_findings" != true ]; then
  say "sift action: reporting only, so the step passes"
  exit 0
fi

exit "$status"
