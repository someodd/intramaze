# Contributing and Repo Guidelines

If you change anything be sure to check that things still build in Docker,
`nix-build`, and `nix-shell`.

If you are contributing, please work within `nix-shell`.

Try to stay within the `nix-shell` to keep all of our environments the same.

Here are some nice things about the Nix shell:

  * `cabal build`: the environment is setup with everything you need to
    consistently build the project with a version of `cabal` it includes
  * `haskell-langauge-server`: you can use in combination with the *Haskell* and
    *Nix Environment Selector* VSCode/VSCodium extensions (formatter, linter,
    documentation, suggestions, etc). Make sure you click "selected" when you
    get the pop up in the lower right with Nix Environment Selector.
  * `postgres`: in case you want to run a postgres server yourself (instead of
    using Docker)
  * `doctest`: so  you can run the doctests in `src`!
  * `cabal2nix`: so you can easily update `default.nix` in case of any changes
    to dependencies
  * `docker` and `docker-compose`, so you can run those commands in the shell
    (see this README's section on Docker)

If you're contributing to this project I'd prefer you to do so within
`nix-shell` and use `nix-build` before you push a commit.

For your editor to use the tools in `nix-shell`, simply launch the editor in the
`nix-shell`. For example, I had to launch `codium` via a terminal inside the
`nix-shell` environment.

## Run and build with cabal

Run cabal in `nix-shell`. Cabal will be a better option than `nix-build` for
fast iteration, testing changes you've made to code before commit. `nix-build`
is better for releases. This is all based off my experience of `nix-build`
taking longer than `cabal build` when making changes.

Build with cabal:

```
cabal build
```

Run with Cabal:

```shell
env SCOTTY_ENV=Test SCOTTY_SITE_TITLE=IntraMaze PORT=8888 SCOTTY_DATABASE_URL=postgres://testpguser:testpguser@localhost:5432/testpgdatabase cabal run
```

## Adding dependencies

Adding a dependency (run in `nix-shell`):

1. Search [searching nixpkgs](https://search.nixos.org/packages) for which
   version of the desired dependency is available
1. Pin that version in `interwebz.cabal`
1. Run `cabal2nix . > default.nix`
1. Try `nix-build release.nix` and `cabal build`

An alternative to the above is (run in `nix-shell`):

1. Just add your dependency boundless in `interwebz.cabal`,
1. `nix-build release.nix`
1. `cabal2nix . > default.nix`
1. look out for which version `nix-build release.nix` installs
1. pin the above version in `interwebz.cabal` (revising the boundless entry).

## Style

Use Fourmolo for formatting. Please see this repo's `fourmolu.yaml`.

## Documentation

  * https://kowainik.github.io/posts/haddock-tips
  * https://haskell-haddock.readthedocs.io/en/latest/

## `git`

Grouping tokens (short tokens prefixing a branch name, like `tooling/nix`)
used:

  * `tooling`: used for anything related to devops, docker, changes that aren't
    specifically related to the software.