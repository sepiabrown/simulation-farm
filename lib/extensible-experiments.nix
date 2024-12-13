{ inputs
, pkgs
, run-dir ? "bank"
}:
let

  inherit (builtins)
    removeAttrs
  ;
  inherit (inputs.nixpkgs.lib)
    flip
    mapAttrsToList
    filterAttrs
    hasAttr
    fixedWidthNumber
    replaceStrings
    makeOverridable
    makeExtensible
    composeManyExtensions
    callPackageWith
    extends
  ;

  extensibleSet = makeExtensible (extends (next: prev: {
    callExperiment = callPackageWith next;
    callExperimentWith = callPackageWith;
  }) (self: {}));

  extensible-experiments = extensibleSet.extend (
    composeManyExtensions [
      (import ./utils.nix { inherit inputs pkgs; })
      (next: prev: {
        run-dir-default = run-dir;

        Host = next.callExperiment ./Host.nix {};
        Register = next.callExperiment ./Register.nix {};
        Experiment = next.callExperiment ./Experiment.nix {};
        Sink = next.callExperiment ./Sink.nix {};
        Measurement = next.callExperiment ./Measurement.nix {};
        Plot = next.callExperiment ./Plot.nix {};
        MultiPlot = next.callExperiment ./MultiPlot.nix {};
        TilePlot = next.callExperiment ./TilePlot.nix {};

        availableShapes = [ "circle" "cross" "stars" "poly3u" "poly3d" "poly4u" "poly4d" "poly5u" "poly5u" "poly6d" "poly6d" ];
        availableColors = ["blue" "red" "green" "gold" "cyan" "magenta" "purple" "brown" "steelblue" "teal" ];

        hosts = {
          hserver7 = next.Host { name = "hserver7"; ip = "h7"; port = "22"; };
          node01 = next.Host { name = "node01"; ip = "n1"; port = "2221"; };
          node02 = next.Host { name = "node02"; ip = "n2"; port = "2222"; };
          node03 = next.Host { name = "node03"; ip = "n3"; port = "2223"; };
          node04 = next.Host { name = "node04"; ip = "n4"; port = "2224"; };
          node05 = next.Host { name = "node05"; ip = "n5"; port = "2225"; };
          localhost = next.Host { name = "localhost"; ip = "localhost"; port = "22"; };
        };
        examples = rec {
          sinks = {
            awk = next.Sink { snippet = "cat <<exid>>.log | ${next.awk} '{print $1, $2}'";};
            log = next.Sink { snippet = "cat <<exid>>.log"; };
          };
          experiments = {
            hello = next.Experiment { exec = "${pkgs.hello}/bin/hello"; };
            echo12 = next.Experiment { exec = "echo"; appArgs = { "001" = "1"; "002" = "2";}; };
            echo23 = next.Experiment { exec = "echo"; appArgs = { "001" = "2"; "002" = "3";}; };
            echo34 = next.Experiment { exec = "echo"; appArgs = { "001" = "3"; "002" = "4";}; };
            echo45 = next.Experiment { exec = "echo"; appArgs = { "001" = "4"; "002" = "5";}; };
          };
          measurements = {
            hello = next.Measurement { host = next.hosts.node01; experiments = [ experiments.hello ] ; sink = sinks.awk; };
            echo12 = next.Measurement { host = next.hosts.node02; experiments = [ experiments.echo12 ]; sink = sinks.log; };
            echo23 = next.Measurement { host = next.hosts.node03; experiments = [ experiments.echo23 ] ; sink = sinks.log; };
            echo123 = next.Measurement { forceRerun = true; host = next.hosts.node03; experiments = [ experiments.echo12 experiments.echo23 ] ; sink = sinks.awk; };
            echo34 = next.Measurement { host = next.hosts.node04; experiments = [ experiments.echo34 ]; sink = sinks.log; };
            echo45 = next.Measurement { host = next.hosts.node05; experiments = [ experiments.echo45 ]; sink = sinks.log; };
          };
          plots = {
            echo123 = next.Plot { name = "echo123"; measurements = with measurements; [ echo123 ]; };
            echo34 = next.Plot { name = "echo34"; measurements = with measurements; [ echo34 echo45 ]; };
          };
          multiplots = {
            echo1234 = next.MultiPlot { plots = with plots; [ echo123 echo34 ]; };
          };
          tiles = {
            echo1234 = next.TilePlot { plots = with plots; [ echo123 echo34 ]; };
          };
        };
      })
    ]);

in extensible-experiments
