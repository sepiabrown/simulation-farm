{ inputs, pkgs, ... }:
let

  inherit (pkgs.lib)
    hasSuffix
    removeSuffix
    makeOverridable
    filterAttrs
    isDerivation
  ;

in
next: prev: with next;
{

  inherit inputs pkgs;

  hplot = "${inputs.hplot.packages.${pkgs.system}.default}/bin/hplot";

  sed = "${pkgs.gnused}/bin/sed";
  awk = "${pkgs.gawk}/bin/awk";
  tee = "${pkgs.coreutils}/bin/tee";
  cat = "${pkgs.coreutils}/bin/cat";
  jq = "${pkgs.jq}/bin/jq";

  echoIf = predicate: content: path: indicator:
    if predicate then ''echo -e "${content}" | ${tee} -a ${path} | ${sed} 's/^/${indicator} /' '' else "";

  mk-hash = x: __hashString "sha256" (__toJSON x);
  snippet-to-script = pkgs.writeShellScript;
  snippet-to-script-exec = name: snippet: "${snippet-to-script name snippet}";
  first6 = str: __substring 0 6 str;
  first7 = str: __substring 0 7 str;
  mk-app = program: { type = "app"; inherit program; };

  withColor = __mapAttrs (_: code: message:
    "\\033[${code}m${message}\\033[0m") {
      black       =  "0;30";    dark_gray    = "1;30";
      red         =  "0;31";    light_red    = "1;31";
      green       =  "0;32";    light_green  = "1;32";
      brown_or_orange=  "0;33"; yellow      = "1;33";
      blue        =  "0;34";    light_blue   = "1;34";
      purple      =  "0;35";    light_purple = "1;35";
      cyan        =  "0;36";    light_cyan   = "1;36";
      light_gray  =  "0;37";    white        = "1;37";
    };


  makeOverridable' = f: orgArgs:
    let overridable = makeOverridable f orgArgs;
    in overridable // rec {
      override = override' (x: x);
      override' = g: makeOverridable' f (orgArgs // (g orgArgs));
    };

  mk-app-recursively =
    let
      drop-non-app = attrSet:
        filterAttrs (name: value: ! (value == {} || isNull value)) attrSet;
      mark-non-app = depth: name: value:
        if depth == 0 then null # Base case: Stop recursion if max depth is reached
        else if name == "app"
        then value
        else if (name == "type" && value == "app") || name == "program"
        then value
        else if __any (s: name == s) [
          # for extensible-experiments
          "pkgs"
          "inputs"

          # for plots
          "scriptExec"
          "snippet"
        ]
        then null
        else if ! __isAttrs value
        then null
        else if isDerivation value
        then null
        else mk-app-recursively (depth - 1) value;
    in
      depth: attrSet: drop-non-app (__mapAttrs (mark-non-app depth) attrSet);

  mk-pkg-recursively =
    let
      drop-non-app = attrSet:
        filterAttrs (name: value: ! (value == {} || isNull value)) attrSet;
      mark-non-app = depth: name: value:
        if depth == 0 then null # Base case: Stop recursion if max depth is reached
        else if __any (s: name == s) [
          # for extensible-experiments
          "pkgs"
          "inputs"
          "override"

          # for plots
          "scriptExec"
          "snippet"
        ]
        then null
        else if isDerivation value
        then value
        else if ! __isAttrs value
        then null
        else mk-pkg-recursively (depth - 1) value;
    in
      depth: attrSet: drop-non-app (__mapAttrs (mark-non-app depth) attrSet);

  removeTrailingSuffix = sfx: str:
    if ! (hasSuffix sfx str)
    then str
    else removeTrailingSuffix sfx (removeSuffix sfx str);

  removeTrailingNewlines = removeTrailingSuffix "\n";

  script-to-app = script-to-app' "script";

  script-to-app' = name: script:
    let
      drv = pkgs.writeShellScript "${name}.sh" script;
    in {
      type = "app";
      program = "${drv}";
    };

  ansi-colored-message = __mapAttrs (_: code: message:
    "\\033[${code}m${message}\\033[0m") {
      black       =  "0;30";    dark_gray    = "1;30";
      red         =  "0;31";    light_red    = "1;31";
      green       =  "0;32";    light_green  = "1;32";
      brown_or_orange=  "0;33"; yellow      = "1;33";
      blue        =  "0;34";    light_blue   = "1;34";
      purple      =  "0;35";    light_purple = "1;35";
      cyan        =  "0;36";    light_cyan   = "1;36";
      light_gray  =  "0;37";    white        = "1;37";
    };
}
