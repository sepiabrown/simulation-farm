{ stdenv, gfortran, ... }:
stdenv.mkDerivation rec {
  pname = "ffi_c";
  version = "0.0";
  src = ./.;

  nativeBuildInputs = [ ];

  buildPhase = ''
    gcc -fPIC -O3 -mavx2 -funroll-loops -ftree-vectorize mmult.c -c -o mmult.o
    gcc -fPIC -O3 -mavx2 -funroll-loops -ftree-vectorize ewm.c -c -o ewm.o
    gcc -fPIC -O3 -mavx2 -funroll-loops -ftree-vectorize rand-vector.c -c -o rand-vector.o -lm
    gcc -shared -O3 -mavx2 -funroll-loops -ftree-vectorize -o libffi_c.so mmult.o ewm.o rand-vector.o -lm
  '';

  installPhase = ''
    mkdir -p $out/lib
    cp libffi_c.so $out/lib
  '';

  installFlags = [
    "DESTDIR=$(out)"
  ];
}
