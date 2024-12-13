{ pkgs, mk-hash, first7  }:
let
  inherit (pkgs.lib)
    hasAttr
    fixedWidthNumber
  ;
in
input:
let
  repeatStr = if hasAttr "repeatID" input then "-${fixedWidthNumber 4 input.repeatID}" else "";
in
rec {
  input' = removeAttrs input [ "repeatID" "forceRerun" ];
  hash = mk-hash input';
  hash7 = first7 hash;
  exid = "${hash}${repeatStr}"; # experiment id
  exid7 = "${hash7}${repeatStr}";
}
