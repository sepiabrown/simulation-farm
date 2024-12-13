{ inputs, pkgs, ... }:
let

  inherit (inputs.nixpkgs.lib) mapAttrsToList flatten;

  hplot = inputs.hplot.packages.${pkgs.system}.default;

  mk-attrs-hash = attrs: __hashString "sha256" (__toJSON attrs);

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


  # data ExperimentSpec = ExperimentSpec {
  #   host :: String,
  #   command :: String
  #   forceRerun :: Bool
  #   tags :: Map String String
  #   envVars :: Map String String
  # }

  # mk-experiment :: ExperimentParam -> ExperimentSpec
  mk-experiment =
    { appPath,
      appArgs ? [],
      host,
      forceRerun ? false,
      envVars ? {},
      tags ? {}
    }:
    let

      appArgStr = __concatStringsSep " " appArgs;

      envVarStr =
        let
          envVarExprs = mapAttrsToList (name: value: "${name}=${value}") envVars;
        in __concatStringsSep " " envVarExprs;

      tagsStr = __concatStringsSep " " tags;
    in
      {
        inherit host forceRerun tags envVars;
        appArgs = appArgStr;
        command = "${envVarStr} ${appPath} ${appArgStr}";
      };

in
{

  inherit
    ansi-colored-message
    mk-attrs-hash
    mk-experiment
    remote-execution-over-ssh
  ;

}
