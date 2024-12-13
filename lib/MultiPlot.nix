{ pkgs, makeOverridable', run-dir-default
, mk-hash, first7, echoIf, tee, sed
, hplot, snippet-to-script, mk-app
}:
let
  inherit (pkgs.lib) flip;
in
arg: makeOverridable' (
  { name ? "plots"
  , title ? "Plots"
  , xlab ? "x"
  , ylab ? "y"
  , xlim ? null
  , ylim ? null
  , xlog ? false
  , ylog ? false
  , plots ? []
  , forceRerun ? false
  , run-dir ? run-dir-default
  }@input:
  let
    input' = (removeAttrs input [ "forceRerun" ]) //
                { plots = map (p: p.hash) plots; };
  in
  rec {

    # idenfications
    hash = mk-hash input';
    hash7 = first7 hash;
    outPath = "${run-dir}/${scriptName}.hx";
    outTemp = "${outPath}.temp";
    svgPath = "${run-dir}/${scriptName}.svg";
    scriptName = "${name}-${hash7}";

    # snippets
    snippet = ''
      if ${checkDone}; then
        echo -e "${outPath} already exists."
        echo -e "${svgPath} already exists."
      else
        ${__concatStringsSep "\n" (flip map plots (p: ''
            ${p.snippet}
          ''))}

        echo
        mkdir -p ${run-dir}
        ${echoIf (name != null) "%title ((${title}))" outTemp ">>"}
        ${echoIf (xlab != null) "%xlab ((${xlab}))" outTemp ">>"}
        ${echoIf (ylab != null) "%ylab ((${ylab}))" outTemp ">>"}
        ${echoIf (xlim != null) "%xlim ${xlim}" outTemp ">>"}
        ${echoIf (ylim != null) "%ylim ${ylim}" outTemp ">>"}
        ${echoIf xlog "%xlog" outTemp ">>"}
        ${echoIf ylog "%ylog" outTemp ">>"}
        echo ">>"
        ${__concatStringsSep "\n" (flip map plots (p: ''
            cat ${p.outPath} \
              | ${tee} -a ${outTemp} \
              | ${sed} 's/^/>> /'
          ''))}

        mv ${outTemp} ${outPath}
        ${hplot} -f ${outPath} -o ${svgPath} &> /dev/null
        echo -e "${outPath} created."
        echo -e "${svgPath} created."
        fi
    '';
    checkDone = ''[ -s "${svgPath}" ] && [ ! "${toString forceRerun}" ]'';

    # scripts
    script = snippet-to-script "${scriptName}_sh" snippet;

    # script execuetables
    scriptExec = "${script}";

    _app = mk-app scriptExec;

  } // input) arg
