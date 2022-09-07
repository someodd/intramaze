let
 pkgs = import (builtins.fetchGit {
    # Descriptive name to make the store path easier to identify
    name = "nixos-main-2022-09-08";
    url = "https://github.com/nixos/nixpkgs/";
    # Commit hash for nixos-unstable as of 2018-09-12
    # `git ls-remote https://github.com/nixos/nixpkgs nixos-unstable`
    ref = "refs/heads/master";
    rev = "7fa06a5398cacc7357819dab136da7694de87363";
  }) {};
in
 pkgs.haskellPackages.callPackage ./default.nix { }
