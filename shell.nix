# This is a work in progress.
# https://input-output-hk.github.io/haskell.nix/tutorials/development.html
let
 pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-haskell-updates-2022-08-08";
    url = "https://github.com/nixos/nixpkgs/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/heads/haskell-updates";
    rev = "e94f52c385b5154d1edd536478553967c900c1c7";
  }) {};
 waybar = pkgs.haskellPackages.callPackage ./default.nix { };
in
 pkgs.haskellPackages.shellFor {
   packages = p: [
     #p.cabal2nix-unstable
     #p.distribution-nixpkgs
     #p.hackage-db
     pkgs.haskellPackages.postgresql-libpq
     waybar
   ];
 
   buildInputs = [pkgs.postgresql pkgs.postgresql.lib];
   systemPackages = [pkgs.postgresql];
   # for running doctests locally
   nativeBuildInputs = [
     pkgs.haskell-language-server
     pkgs.haskellPackages.doctest
     pkgs.haskellPackages.ghcide
     pkgs.cabal2nix
     #pkgs.haskellPackages.Cabal_3_6_3_0
     pkgs.cabal-install
     #pkgs.libjwt
     pkgs.postgresql
     pkgs.postgresql.lib
   ];

   # set environment variable, so the development version of
   # distribution-nixpkgs finds derivation-attr-paths.nix
   distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
 }
