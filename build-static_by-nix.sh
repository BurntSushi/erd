#!/usr/bin/env bash

# This script creates a static executable of `erd` by using `nix` package
# manager with the help of `static-haskell-nix`.
# https://github.com/nh2/static-haskell-nix

# Nix is a requirement for this to build successfully.
# https://nixos.org/nix/

# The resulted executable will be copied into `pwd` once executed. Building time
# can be long depending on already built (cached) packages.

if [ `command -v nix-build` ]; then
    cp $($(nix-build --no-link -A fullBuildScript))/bin/erd .
else
    echo "The command \`nix-build\` does not seem to exists on your system."
    echo "Please install it first to run this script!"
    exit 1
fi

