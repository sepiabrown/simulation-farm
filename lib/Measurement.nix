{ pkgs, makeOverridable', mk-hash, first7
, tee, sed, withColor, snippet-to-script
, removeTrailingNewlines}:
let
  inherit (pkgs.lib) flip;
in
arg: makeOverridable' (
  { name ? "Measurement"
  , host ? "localhost"
  , experiments ? []
  , sink
  , forceRerun ? false
  }@input:
  let
    experiments' = map (e: e // { hostName = host.name; }) experiments;
    experimentExids = map (e: e.exid) experiments';
  in
  rec {

    # hash for idenitification
    hash = mk-hash [ name host.hash experimentExids sink.hash ];
    hash7 = first7 hash;
    outPath = "${name}-${hash}.mea";
    outTemp = "${outPath}.temp";
    scriptName = "${name}-${hash7}";

    snippet = host.liftSnippet ''
      if ${checkDone}; then
          echo "${outPath} already exists"
      else
        ${__concatStringsSep "\n\n" (flip map experiments' (e: ''
          ${e.snippet}
          ${removeTrailingNewlines (pkgs.lib.escape [ ] (sink.snippetWithExid e.exid))} \
            | ${tee} -a ${outTemp}
          ''))}
        mv ${outTemp} ${outPath}
      fi
      echo -e "${withColor.yellow "Measured:"}"
      cat ${outPath}
    '';
    snippetCat = host.liftSnippet "cat ${outPath}";
    checkDone = ''[ -s ${outPath} ] && [ ! "${toString forceRerun}" ]'';

    # continuations
    snippetCont = f: f snippet;
    scriptExecCont = f: f scriptExec;
    scriptCont = f: snippet-to-script "${scriptName}_sh" (f snippet);

    # scripts
    script = snippet-to-script "${scriptName}_sh" snippet;
    scriptCat = snippet-to-script "cat--${scriptName}_sh" snippetCat;

    # script execuetables
    scriptExec = "${script}";
    scriptExecCat = "${scriptCat}";

  } // input // { experiments = experiments'; }) arg
