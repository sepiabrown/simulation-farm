{ pkgs, makeOverridable', mk-hash, first7, tee, sed, awk
, withColor, snippet-to-script, snippet-to-script-exec }:
let
  inherit (pkgs.lib)
    flip
    replaceStrings
  ;
in
arg: makeOverridable' (
  { name ? "Sink"
  , exidPlaceHolder ? "<<exid>>"
  , snippet ? "cat ${exidPlaceHolder}.log | ${awk} '{print $1, $2}'"
  }@input:
  rec {
    hash = mk-hash input;
    hash7 = first7 hash;
    snippetWithExid = exid: replaceStrings [ exidPlaceHolder ] [ exid ] snippet;
    hashWithExid = exid: mk-hash (input // { snippet = snippetWithExid exid;});
    hash7WithExid = exid: first7 (hashWithExid exid);
    scriptWithExid = exid: snippet-to-script "${name}-${hash7WithExid exid}_sh" (snippetWithExid exid);
    scriptExecWithExid = exid: snippet-to-script-exec "${name}-${hash7WithExid exid}_sh" (snippetWithExid exid);
  } // input) arg
