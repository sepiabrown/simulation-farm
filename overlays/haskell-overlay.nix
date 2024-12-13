{ llvm ? "nollvm" }: next: prev:
let
  mkLLVM = x:
    if llvm == "nollvm"
      then x
      else
        let
          ver = prev.lib.removePrefix "llvm" llvm;
        in (prev.haskell.lib.appendConfigureFlag x "--ghc-options=-fllvm").overrideAttrs (old: {
             buildInputs = old.buildInputs ++ [ prev."llvmPackages_${ver}".llvm ];
           });
in
{

  haskell = prev.haskell // {
    packageOverrides = prev.lib.composeManyExtensions [
      (prev.haskell.packageOverrides or (_: _: {}))
      (hself: hsuper: {
        ghc = if ! isNull (__match "-binary-" hsuper.ghc.meta.name)
              then hsuper.ghc.override { useLLVM = if llvm != "nollvm" then true else false; }
              else hsuper.ghc;
        logging = mkLLVM (hself.callCabal2nix "logging" ../benchmarks/logging {});
        parFFI = mkLLVM (hself.callCabal2nix "parFFI" ../benchmarks/parFFI {});
        vecmat = mkLLVM (hself.callCabal2nix "vecmat" ../benchmarks/vecmat {});
        parHmatrix = mkLLVM (hself.callCabal2nix "parHmatrix" ../benchmarks/parHmatrix {});
        parBlas = mkLLVM (hself.callCabal2nix "parBlas" ../benchmarks/parBlas {});
        parConduit = mkLLVM (hself.callCabal2nix "parConduit" ../benchmarks/parConduit {});
      })
    ];
  };

}
