{ inputs, pkgs }:
let

  pkgs' = pkgs.extend (import ./overlays/blas-overlay.nix { blasType = "obavx"; useLLVM = false;} );
  hplot = inputs.hplot.packages.${pkgs.system}.default;

in
with pkgs';
haskellPackages.shellFor {
  withHoogle = true;
  packages = p: with p; [
    logging
    parBlas
    parFFI
    parHmatrix
    vecmat
    parConduit
  ];
  buildInputs =
    (with haskellPackages; with haskell.lib;
    [ haskell-language-server
      ghcid
      threadscope
    ]) ++
    [ ghcid.bin
      cabal-install
      speedscope
      heapscope
      hplot
    ];
}
