{ inputs, pkgs, }:
let

  extensible-experiments = import ../lib/extensible-experiments.nix { inherit inputs pkgs; };

in
{

  lib = extensible-experiments;

  inherit (extensible-experiments) examples;

  parConduit = import ./parConduit.nix { inherit inputs pkgs extensible-experiments; };

}
