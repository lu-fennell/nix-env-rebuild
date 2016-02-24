#!/usr/bin/env bash
set -e

# Build shell.nix and default.nix
cabal2nix . > default.nix
cabal2nix --shell . > shell.nix

# Make sure the tests work
nix-shell --run 'cabal test'

# Make a release tarball
nix-shell --run 'cabal build'
nix-shell --run 'cabal sdist'

# Try to build the package with nix
nix-build -E 'with (import <nixpkgs> {}); with haskellPackages; callPackage ./. {}'

echo "=============="
echo "All seems well!"
