# sift --- a dialect-aware formatter, linter and language server for Scheme,
# Emacs Lisp and Common Lisp.
#
# SPDX-License-Identifier: MIT
# Copyright © Josep Bigorra <jjbigorra@gmail.com>
#
# A flake for the checkout, so that a machine with Nix -- Linux or macOS, which
# is the half of the audience `guix install' never reached -- can build sift,
# run it, and get a shell with it without installing Guile first:
#
#   nix run git+https://github.com/jjba23/sift -- version
#   nix build .#sift && ./result/bin/sift check .
#   nix develop          # a shell with sift on PATH and the library on it
#
# The derivation is `default.nix`, which is also what the nix-jointhefreeworld
# flake calls: this file is a way in, not a second expression that could drift
# from the first.
#
# `packages.<system>.docker` is the reproducible image, built the way Nix builds
# one -- a layer per store path, no daemon, no Dockerfile.  It is a Linux
# artefact (an image is), so it exists on the Linux systems and is simply absent
# on Darwin, where the `sift` package and the app still are.  The Dockerfile at
# the root of the checkout is the other way to the same place, for a machine
# with an engine and no Nix.

{
  description = "sift — a formatter, linter and language server for Scheme, Emacs Lisp and Common Lisp";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs = { self, nixpkgs }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      linuxSystems = [ "x86_64-linux" "aarch64-linux" ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
      forLinux = nixpkgs.lib.genAttrs linuxSystems;

      pkgsFor = system: nixpkgs.legacyPackages.${system};

      # One callPackage, in one place: the version is read from the sources by
      # the derivation itself, so nothing here restates it.
      siftFor = system: (pkgsFor system).callPackage ./default.nix { };

      dockerFor = system:
        let
          pkgs = pkgsFor system;
          sift = siftFor system;
        in
        pkgs.dockerTools.buildLayeredImage {
          name = "jjba23/sift";
          tag = "latest";
          contents = [ sift pkgs.cacert ];
          # The launcher is the entry point, so `docker run … check .` is the
          # command line; the workdir is where a volume is expected to land.
          config = {
            Entrypoint = [ "${sift}/bin/sift" ];
            Cmd = [ "--help" ];
            WorkingDir = "/src";
            Labels = {
              "org.opencontainers.image.title" = "sift";
              "org.opencontainers.image.source" = "https://github.com/jjba23/sift";
              "org.opencontainers.image.licenses" = "MIT";
            };
          };
        };
    in
    {
      packages = forAllSystems (system:
        let sift = siftFor system; in
        {
          inherit sift;
          default = sift;
        }
        // nixpkgs.lib.optionalAttrs (builtins.elem system linuxSystems) {
          docker = dockerFor system;
        });

      apps = forAllSystems (system: {
        sift = {
          type = "app";
          program = "${self.packages.${system}.sift}/bin/sift";
        };
        default = self.apps.${system}.sift;
      });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
        in
        {
          default = pkgs.mkShell {
            inputsFrom = [ self.packages.${system}.sift ];
            packages = [ pkgs.guile ];
          };
        });

      checks = forAllSystems (system: {
        sift = self.packages.${system}.sift;
      });

      formatter = forAllSystems (system: (pkgsFor system).nixfmt-rfc-style);
    };
}
