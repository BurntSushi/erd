# This script may be rather called by the `builder-nix.sh` which places the
# resulted executable into `pwd`.

# However, when called standalone: the following command will build `erd` and
# would return with path of the built `erd` project, from where the executable
# would be available.
#
#     $(nix-build --no-link -A fullBuildScript)
#

# Original source is from:
# https://github.com/nh2/static-haskell-nix/tree/master/static-stack2nix-builder-example
#

{
  stack2nix-output-path ? "custom-stack2nix-output.nix",
}:
let
  cabalPackageName = "erd";
  compiler = "ghc864"; # matching stack.yaml

  # Pin static-haskell-nix version.
  static-haskell-nix =
    if builtins.pathExists ../.in-static-haskell-nix
      # for the case that we're in static-haskell-nix itself, so that CI always
      # builds the latest version. Update this hash to use a different
      # `static-haskell-nix` version:

      then toString ../.

      else fetchTarball https://github.com/nh2/static-haskell-nix/archive/475739275fbdfce025cd8a0e83f79164a7cdc66e.tar.gz;

  # Pin nixpkgs version By default to the one `static-haskell-nix` provides, but
  # you may also give your own as long as it has the necessary patches, using
  # e.g.
  #     pkgs = import (fetchTarball https://github.com/nh2/nixpkgs/archive/xyz.tar.gz) {};

  pkgs = import "${static-haskell-nix}/nixpkgs.nix";

  stack2nix-script = import "${static-haskell-nix}/static-stack2nix-builder/stack2nix-script.nix" {
    inherit pkgs;
    stack-project-dir = toString ./.; # where stack.yaml is
    hackageSnapshot = "2019-11-10T00:00:00Z"; # pins e.g. extra-deps without hashes or revisions
  };

  static-stack2nix-builder = import "${static-haskell-nix}/static-stack2nix-builder/default.nix" {
    normalPkgs = pkgs;
    inherit cabalPackageName compiler stack2nix-output-path;
    # disableOptimization = true; # for compile speed
  };

  # Full invocation, including pinning `nix` version itself.
  fullBuildScript = pkgs.writeScript "stack2nix-and-build-script.sh" ''
    #!/usr/bin/env bash
    set -eu -o pipefail
    STACK2NIX_OUTPUT_PATH=$(${stack2nix-script})
    export NIX_PATH=nixpkgs=${pkgs.path}
    ${pkgs.nix}/bin/nix-build --no-link -A static_package \
        --argstr stack2nix-output-path "$STACK2NIX_OUTPUT_PATH" "$@"
  '';

in
  {
    static_package = static-stack2nix-builder.static_package;
    inherit fullBuildScript;
    # For debugging:
    inherit stack2nix-script;
    inherit static-stack2nix-builder;
  }
