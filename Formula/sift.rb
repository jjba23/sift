# sift --- a dialect-aware formatter and linter for Scheme, Racket, Emacs Lisp
# and Common Lisp.
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# The Homebrew formula for the tap this repository is:
#
#   brew tap jjba23/sift https://github.com/jjba23/sift
#   brew install jjba23/sift/sift
#
# A formula and not a cask: a cask ships a prebuilt binary, and sift has
# nothing to build -- it is Guile source and a shell launcher, so the package
# is a copy and the `depends_on "guile"` is the whole of the platform work.
# There is no gem, and none is wanted: this is not a Ruby program.
#
# The launcher finds the library beside itself, so the two are installed in the
# relation a checkout has (bin/ next to src/) inside libexec, and bin/sift is
# the symlink the launcher already knows how to resolve.  The one edit is the
# interpreter: `exec guile' would run whichever Guile PATH holds, and the
# dependency is the one to run, which is the same substitution the Nix and Guix
# packages make.

class Sift < Formula
  desc "Dialect-aware formatter, linter and language server for Lisp"
  homepage "https://github.com/jjba23/sift"
  url "https://github.com/jjba23/sift/archive/refs/tags/v1.34.0.tar.gz"
  sha256 "d7411a6c7f2dbdff67360f0cfc983b8150bfb57fb21d8974769a1b0036e38e8f"
  license "MIT"

  depends_on "guile"

  def install
    libexec.install "bin", "src"
    guile = Formula["guile"].opt_bin/"guile"
    inreplace libexec/"bin/sift", "exec guile", "exec #{guile}"
    bin.install_symlink libexec/"bin/sift"
  end

  test do
    assert_match "sift #{version}", shell_output("#{bin}/sift version")
    # The cheapest end-to-end check there is: the tool reads a file with
    # sift's own reader and finds nothing to say about it.
    (testpath/"formatted.scm").write("(define (f) 1)\n")
    system bin/"sift", "check", "formatted.scm"
  end
end
