{ pkgs, mk-hash, run-dir-default, makeOverridable', ... }:
let

  inherit (pkgs.lib) replaceStrings;
  inherit (pkgs) writeStringReferencesToFile;

in
arg: makeOverridable' (
  { name ? "localhost"
  , ip ? "localhost"   # when ip is "localhost", remote run is disabled
  , port ? "22"
  , user ? "hds"
  , ssh-option ? "-p ${port} -o StrictHostKeyChecking=No -o LogLevel=ERROR -o UserKnownHostsFile=/dev/null"
  , ssh-command ? "ssh ${ssh-option}"
  , run-dir ? run-dir-default
  }@input:
  let
    store-uri = "ssh-ng://${user}@${ip}";
    ssh-command-full = ''${ssh-command} "${user}@${ip}" -T '';
  in
  rec {

    hash = mk-hash input;

    liftSnippet = snippet:
      if ip == "localhost" then ''
        mkdir -p ${run-dir}
        cd ${run-dir}
        ${snippet}
      ''
      else
      let
        indent = "\t";
        references = writeStringReferencesToFile snippet;
        snippet' = replaceStrings [ "$(date)" ] [ "\\$(date)" ] snippet; # \$ make variable expansion happen at host
      in  ''
        NIX_SSHOPTS="${ssh-option}" \
        nix copy ${references} \
            --to "${store-uri}" \
            --no-check-sigs

        ${ssh-command-full} <<EOF${hash}
          mkdir -p ${run-dir}
          cd ${run-dir}
          ${snippet'}
        EOF${hash}'';

    liftSnippetOneLine = snippet: "${ssh-command-full} ${snippet}";

    checkExist = path: '' ${ssh-command-full} '[ -s "${path}" ]' '';

    copy = path: ''tar czf - ${path} | ${ssh-command-full} "mkdir -p ${run-dir} && cd ${run-dir} && tar xzf -"'';

  } // input) arg
