{
  description = "Declaring Reproducible Simulation Farm Using Nix";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/0fc9fca9c8d43edd79d33fea0dd8409d7c4580f4;
    hplot.url = git+ssh://git@github.com/haedosa/hplot?ref=xylim-logaxis;
    hutils.url = git+ssh://git@github.com/haedosa/hutils?ref=conduit-parallel;
  };


  outputs = inputs@{ self, ... }:
    let
      system = "x86_64-linux";
      pkgs = import inputs.nixpkgs {
        inherit system;
        config = { allowUnfree = true; };
        overlays = [
          inputs.hutils.overlay
          (import ./overlays/haskell-overlay.nix { llvm = "llvm12"; })
        ];
      };
      inherit (inputs.nixpkgs.lib) mapAttrsToList flatten;

      sims = import ./experiments { inherit inputs pkgs; };

    in
      rec {

        inherit sims;

        devShells.${system} = {
          default = import ./develop.nix { inherit inputs pkgs; };
        };

        packages.${system} = sims.lib.mk-pkg-recursively 100 sims;

        apps.${system} = sims.lib.mk-app-recursively 100 sims;

        # hydraJobs.runCommandHook = self.packages.${system};
        hydraJobs.runCommandHook.parConduit.plot-tile-llvm.script = self.packages.${system}.parConduit.plot-tile-llvm.script;

      };
}
