{ inputs, pkgs, sims }:
let

  inherit (builtins) hasAttr;

  inherit (pkgs.lib)
    mapAttrs
  ;

  inherit (sims.lib)
    script-to-app
    ansi-colored-message
    mk-app-recursively
  ;

  hpc-ips = {
    node01 = "20.20.100.1";
    node02 = "20.20.100.1";
    node03 = "20.20.100.1";
    node04 = "20.20.100.1";
    node05 = "20.20.100.1";
  };

  hpc-port = {
    node01 = "2221";
    node02 = "2222";
    node03 = "2223";
    node04 = "2224";
    node05 = "2225";
  };

  run-dir = "evaluations";

  mk-app-run-over = node-name: script: let
      ip = hpc-ips.${node-name};
      ssh-option = ''-p ${hpc-port.${node-name}} -o StrictHostKeyChecking=No -o UserKnownHostsFile=/dev/null'';
      ssh-command = ''ssh ${ssh-option}'';
      store-uri = "ssh-ng://hds@${ip}";
      references = pkgs.writeStringReferencesToFile script;
    in
      script-to-app ''

        echo -e "${ansi-colored-message.purple "hpc: ${node-name} "}"

        NIX_SSHOPTS="${ssh-option}" \
        nix copy ${references} \
            --to "${store-uri}" \
            --no-check-sigs

        ${ssh-command} "hds@${ip}" <<EOF
          mkdir -p ${run-dir}
          cd ${run-dir}
          ${script}
        EOF
      '';


in mk-app-recursively 100 sims
