# sift --- a dialect-aware formatter and linter for Scheme, Emacs Lisp and
# Common Lisp.
#
# SPDX-License-Identifier: GPL-3.0-or-later
#
# A derivation for sift, written once and called from wherever it is needed:
#
#   nix-build -E 'with import <nixpkgs> {}; callPackage ./default.nix { }'
#   callPackage ./default.nix { }    # from another expression or a flake
#
# `callPackage' passes `guile' and nothing else, and the source defaults to this
# directory, so this one file packages a checkout, a `git+https' input and the
# tarball of a release without a second expression that could drift from it.
#
# The argument is called `source' rather than `src' because `callPackage' fills
# an argument named `src' from nixpkgs -- where it is a revision control system
# -- and a package that is silently built from the wrong tree is a package that
# fails in a way nobody can read.
#
# The launcher in bin/sift adds the directory it lives in to Guile's load path,
# which is why the modules are installed under share/guile/site/3.0: that is
# the layout `maak install' produces, so an installed sift finds its own library
# the same way however it was installed.

{ lib
, stdenv
, guile
, source ? ./.
}:

let
  # The version is stated once in the sources and quoted from there, so this
  # file reads it rather than restating it.
  records = builtins.readFile "${source}/src/sift/records.scm";
  after = builtins.head (builtins.tail
    (lib.splitString "(define sift-version \"" records));
  version = builtins.head (builtins.split "\"" after);
in
stdenv.mkDerivation {
  pname = "sift";
  inherit version;
  src = source;

  nativeBuildInputs = [ guile ];
  buildInputs = [ guile ];

  # sift is a library and a launcher: there is nothing to compile, and a build
  # that compiled the modules would only produce a second copy of them.  What
  # is worth checking is that the tool runs and can read its own source, which
  # is the cheapest end-to-end test there is.
  doCheck = true;
  checkPhase = ''
    runHook preCheck
    ./bin/sift version
    ./bin/sift check src/sift/records.scm
    runHook postCheck
  '';

  installPhase = ''
    runHook preInstall

    site=$out/share/guile/site/3.0
    install -d "$site" "$out/bin"
    cp -r src/sift "$site/sift"

    install -m 755 bin/sift "$out/bin/sift"
    substituteInPlace "$out/bin/sift" \
      --replace-fail '$here/../src' "$site" \
      --replace-fail 'exec guile' "exec ${guile}/bin/guile -C $out/lib/guile/3.0/site-ccache"

    runHook postInstall
  '';

  # The compiled modules, in the place Guile looks for them and compiled from
  # the installed sources, so that the paths compiled into them are the paths
  # they are found at.  Without this, every first run of sift compiles sixteen
  # modules into the user's cache before it answers.
  postInstall = ''
    ccache=$out/lib/guile/3.0/site-ccache
    for source in $(find "$out/share/guile/site/3.0" -name '*.scm' | sort); do
      target="$ccache/''${source#"$out/share/guile/site/3.0/"}"
      mkdir -p "$(dirname "''${target%.scm}.go")"
      guild compile -L "$out/share/guile/site/3.0" \
        -o "''${target%.scm}.go" "$source"
    done
  '';

  meta = {
    description = "Dialect-aware formatter and linter for Scheme, Emacs Lisp and Common Lisp";
    longDescription = ''
      sift formats and lints Scheme, Emacs Lisp and Common Lisp without losing
      a byte: it reads every comment, blank line and reader prefix into a node
      of its own, so formatting is reversible and idempotent, and its checkers
      are documented, independently testable and aware of which dialect they
      are looking at.
    '';
    homepage = "https://github.com/jjba23/sift";
    license = lib.licenses.gpl3Plus;
    mainProgram = "sift";
    platforms = lib.platforms.unix;
  };
}
