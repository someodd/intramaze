# Contributing and Repo Guidelines

If you change anything be sure to check that things still build in Docker,
`nix-build`, and `nix-shell`.

## `git`

Grouping tokens (short tokens prefixing a branch name, like `tooling/nix`)
used:

  * `tooling`: used for anything related to devops, docker, changes that aren't
    specifically related to the software.

## Adding a dependency

Make sure to pin a specific version of the dependency you want in the
`interwebz.cabal` and make sure that such version available in the pinned
nixpkgs version (see `shell.nix` or `release.nix`). I recommend just following
this process:

  1. [Search nixpkgs for the dependency you want to add](https://search.nixos.org/packages)
  1. Add dependency without bounds to `interwebz.cabal` (if it's a Haskell
     package? what about system libraries that can go in the cabal file too?)
  1. If you added a Haskell package dependency then run: `nix-shell --packages cabal2nix --run "cabal2nix ." > default.nix`
  1. Run `nix-build` and see which version of the package it resolves for the
     dependency and pin that version in `interwebz.cabal` (if a Haskell package, I guess)