{ stdenv, gfortran, myblas, ... }:
stdenv.mkDerivation rec {
  pname = "mycblas";
  version = "0.0";
  src = ./.;

  nativeBuildInputs = [ gfortran myblas ];

  buildPhase = ''
    gcc -fPIC -O3 -mavx2 -funroll-loops -ftree-vectorize dgemmc.c -c -o dgemmc.o
    gcc -fPIC -O3 -mavx2 -funroll-loops -ftree-vectorize vector-aux.c -c -o vector-aux.o
    gcc -shared -O3 -mavx2 -funroll-loops -ftree-vectorize -o libmycblas.so dgemmc.o vector-aux.o -lblas -lgfortran
    gcc -o test test.c -lblas
  '';

  # buildPhase = ''
  #   gcc -fPIC -O2 mmult.c -c -o mmult.o
  #   gcc -shared -O2 -o libffi_c.so mmult.o
  # '';

  installPhase = ''
    mkdir -p $out/lib
    mkdir -p $out/bin
    cp libmycblas.so $out/lib
    cp test $out/bin
  '';

  installFlags = [
    "DESTDIR=$(out)"
  ];
}
