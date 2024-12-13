{ pkgs, tee, cat, Register, makeOverridable', withColor, jq, mk-app, ... }:
let

  inherit (builtins) removeAttrs;
  inherit (pkgs.lib)
    mapAttrsToList
  ;


in
arg: makeOverridable' (
    { name ? "Expermiment"
    , hostName ? "localhost"
    , exec ? "${pkgs.hello}/bin/hello"
    , appArgs ? {}  # key assumed to be sorted accordingly , e.g. { "01-NumData" = "100"; "02-SizeChunk" = "3";  }
    , rtsOpts ? {}  # e.g. { "H" = "512m"; "N" = "3";  }
    , postFix ? "2>&1"
    , repeatID ? 1
    , forceRerun ? false
    , envVars ? {}
    , tags ? {}
    }@input:
    let

      inherit (Register input)
        hash
        exid
        exid7
        input'
      ;

      inputJSON = __toJSON input';

      appArgStr = __concatStringsSep " " (__attrValues appArgs);
      rtsOptsStr = if rtsOpts == {}
        then ""
        else "+RTS " + (__concatStringsSep " " (mapAttrsToList (k: v: "-${k}${v}") rtsOpts)) + " -RTS";
      envVarStr =
        let
          envVarExprs = mapAttrsToList (name: value: "${name}=${value}") envVars;
        in __concatStringsSep " " envVarExprs;


      command = "${envVarStr} ${exec} ${appArgStr} ${rtsOptsStr} ${postFix}";
      infile = "${exid}.input.json";
      logfile = "${exid}.log";
      metafile = "${exid}.meta.json";

      snippet = ''
        echo -e "${withColor.purple "Experiment ${exid}"}"

        echo -e "${withColor.cyan "Input:"}"
        if [ ! -s ${infile} ]; then
          echo -e "${pkgs.lib.escape ["\""] inputJSON}" \
            | ${jq} \
            | ${tee} ${infile}
        else
          ${cat} ${infile}
        fi

        if ${checkDone}; then
          echo -e "${withColor.yellow "Log:"}"
          ${cat} ${logfile}
          echo -e "${withColor.cyan "Meta:"}"
          ${cat} ${metafile}
        else
          echo -e "${withColor.yellow "Running:"}"
          ${command} | ${tee} ${logfile}.tmp
          mv ${logfile}.tmp ${logfile}

          echo -e "${withColor.cyan "Meta:"}"
          echo '{}' \
             | ${jq} ". += { \"user\" : \"$USER\" }" \
             | ${jq} ". += { \"hostname\" : \"$HOSTNAME\" }" \
             | ${jq} ". += { \"date\" : \"$(date)\" }" \
             | ${tee} ${metafile}
        fi
      '';
      checkDone = ''[ -s ${logfile} ] && [ ! "${toString forceRerun}" ]'';
      snippetCont = f: f snippet;

      scriptName = "${name}-${exid7}";
      scriptExec = "${script}";
      script = pkgs.writeShellScript "${scriptName}_sh" snippet;

      _app = mk-app scriptExec;

    in
    { inherit
        hash
        exid
        exid7
        command
        script
        snippet
        checkDone
        scriptName
        scriptExec
      ;
    } // input) arg
