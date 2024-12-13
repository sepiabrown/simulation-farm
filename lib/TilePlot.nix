{ pkgs, makeOverridable', run-dir-default
, mk-hash, first7, echoIf, tee, sed
, hplot, snippet-to-script, mk-app
}:
let
  inherit (builtins) length;
  inherit (pkgs.lib) flip;
  inherit (pkgs) imagemagick liberation_ttf;

  font = "${liberation_ttf}/share/fonts/truetype/LiberationSans-Regular.ttf";
  montage = "${imagemagick}/bin/montage -font ${font}";

in
arg: makeOverridable' (
  { name ? "tile"
  , title ? "Tile"
  , plots ? []
  , layout ? let len = length plots; in if len <= 4 then "${toString len}x" else "4x"
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
    outPath = "${run-dir}/${scriptName}.pdf";
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
        ${montage} -mode concatenate -geometry +0+0 -tile ${layout} ${__concatStringsSep " " (map (p: p.svgPath) plots)} ${outPath}

        convert ${outPath} ${svgPath}
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
