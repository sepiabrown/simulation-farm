{ pkgs, makeOverridable', run-dir-default
, mk-hash, first7, echoIf, tee, sed, hplot
, snippet-to-script, mk-app
}:
let
  inherit (pkgs.lib)
    flip
  ;
in
arg: makeOverridable' (
  { name ? "plot"
  , label ? name
  , shape ? null
  , color ? null
  , measurements ? [ ]
  , run-dir ? run-dir-default
  , forceRerun ? false
  }@input:
  let
    input' = (removeAttrs input [ "forceRerun" ]) //
                { measurements = map (m: m.hash) measurements; };
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
        ${__concatStringsSep "\n" (flip map measurements (m: ''
          ${m.scriptExec}
          ''))}

        pwd
        whoami
        ls -al
        cat /etc/hostname
        mkdir -p ${run-dir}
        ${echoIf (label != null) ":label ((${label}))" outTemp ">"}
        ${echoIf (shape != null) ":shape ((${shape}))" outTemp ">"}
        ${echoIf (color != null) ":color ((${color}))" outTemp ">"}
        ${__concatStringsSep "\n" (flip map measurements (m: ''
            ${m.scriptExecCat} \
              | ${tee} -a ${outTemp} \
              | ${sed} 's/^/> /'
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
