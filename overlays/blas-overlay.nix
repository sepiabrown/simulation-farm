{blasType ? "ob", useLLVM ? false }:
let
  blasVars =
  {
    ob =     { blasName = "openblas"; args = { enableAVX512 = false; }; };
    # obs =    { blasName = "openblas"; args = { singleThreaded = true;  enableAVX512 = false; }; };
    obavx =  { blasName = "openblas"; args = { enableAVX512 = true; };  };
    # obavxs = { blasName = "openblas"; args = { singleThreaded = true;  enableAVX512 = true; };  };
    ab =     { blasName = "amd-blis"; args = { }; };
    # abs =    { blasName = "amd-blis"; args = { withOpenMP = false; }; };
    mkl =    { blasName = "mkl";      args = { }; };
  };

  blasName = blasVars."${blasType}".blasName;
  args = blasVars."${blasType}".args;

in
next: prev:
{

  myblas = prev.blas.override { blasProvider = next."${blasName}".override args; };
  mycblas = next.callPackage ../benchmarks/packages/mycblas {};
  # openblas = prev.openblas.overrideAttrs (old: {
  #   makeFlags = map (value: if value == "NUM_THREADS=64" then "NUM_THREADS=128" else value) old.makeFlags;
  # });

  ffi_c = next.callPackage ../benchmarks/packages/ffi_c {};

} // (import ./haskell-overlay.nix { llvm = "nollvm"; } next prev)
