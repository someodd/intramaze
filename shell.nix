let
 pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-haskell-updates-2022-08-08";
    url = "https://github.com/nixos/nixpkgs/";
    # Found using `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/heads/haskell-updates";
    rev = "e94f52c385b5154d1edd536478553967c900c1c7";
  }) {};
 waybar = pkgs.haskellPackages.callPackage ./default.nix { };
in
 pkgs.haskellPackages.shellFor {
   packages = p: [
     pkgs.haskellPackages.postgresql-libpq
     waybar
   ];
 
   buildInputs = [pkgs.postgresql pkgs.postgresql.lib];
   systemPackages = [pkgs.postgresql];

   nativeBuildInputs = [
     pkgs.haskellPackages.haskell-language-server
     pkgs.haskellPackages.doctest
     pkgs.docker-compose
     pkgs.docker
     pkgs.haskellPackages.ghcide
     pkgs.cabal2nix
     pkgs.cabal-install
     pkgs.postgresql
     pkgs.postgresql.lib
   ];

   # set environment variable, so the development version of
   # distribution-nixpkgs finds derivation-attr-paths.nix
   distribution_nixpkgs_datadir = toString ./distribution-nixpkgs;
 }
