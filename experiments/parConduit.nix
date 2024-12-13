{ inputs, pkgs, extensible-experiments }:
let

  inherit (builtins) length;

  inherit (inputs.nixpkgs.lib)
    flip
    filterAttrs
    hasPrefix
    hasAttr
    attrValues
    concatLists
  ;

  inherit (pkgs.lib)
    range
    cartesianProductOfSets
  ;

  inherit (extensible-experiments)
    Experiment
    Sink
    Measurement
    Plot
    MultiPlot
    TilePlot
    hosts
  ;

  pkgsFor = __listToAttrs (map (llvm: {
    name = llvm;
    value = pkgs.extend (import ../overlays/haskell-overlay.nix { inherit llvm; });
  }) [ "nollvm"
       "llvm11"
       "llvm12"
       "llvm13" # failed to build
       "llvm14"
     ]);

  sinks = {
    ncore-time = Sink
      { snippet = ''
          cat <<exid>>.log \
            | awk '/numCapability/{print $NF} (/^3/ && /took/){print $NF}' \
            | xargs -L 2 echo \
            | sed 's/s//' \
            | awk '{print $1, $2}' '';
      };
  };


  default = extensible-experiments.extend (next: prev: {

    parameters = {
      ghc = "924";
      model = "Sleep";  # "Reduce" "Ponder" "ReduceCompact" "NormVector"
      parallelizer = "ConduitMVar"; # "CondutiSTM" "Concurrently" "ParList"
      sizeChunk = 1;
      numData = 1;
      numCore = 1;
      workLoad = 10000;
      heapSize = "512m";
      llvm = "nollvm";
      host = hosts.localhost;
      sink = sinks.ncore-time;
      qa = false;
      forceRerun = false;
      extraCore = 2;
    };

    pkgs = pkgsFor."${next.parameters.llvm}";

    experiment = next.callExperiment (pars:
      (Experiment {
        name = "parConduit";
        exec = "${pkgsFor."${pars.llvm}".haskell.packages."ghc${pars.ghc}".parConduit}/bin/compute";
        appArgs = {
          "01_model" = toString pars.model;
          "02_numData" = toString pars.numData;
          "03_sizeChunk" = toString pars.sizeChunk;
          "04_workLoad" = toString pars.workLoad;
          "05_parallelizer" = toString pars.parallelizer;
        };
        envVars = {
          MKL_NUM_THREADS = (toString 1);
          OPENBLAS_NUM_THREADS = (toString 1);
          OMP_NUM_THREADS = (toString 1);
        };
        rtsOpts = {
          "A" = pars.heapSize;
          "N" = toString pars.numCore;
        } // (if pars.qa then { qa = ""; } else {});
        inherit (pars) forceRerun;
      })) next.parameters;


    mkNumDataCore = p: n: if p == "ConduitCapPool2"
                          then { numData = n; numCore = n + 1 + next.parameters.extraCore; }
                          else { numData = n; numCore = n + next.parameters.extraCore; };
    # mkNumDataCore = p: n: { numData = n; numCore = n + 2; };

    plot = next.callExperiment (pars:
      Plot ({
        inherit (pars) label forceRerun;
        measurements = [
          (Measurement {
            inherit (pars) host sink forceRerun;
            experiments =
              flip map pars.xs (n:
                  # (next.experiment.override (pars // { numData = n; numCore = n; })));
                  (next.experiment.override (pars // ( next.mkNumDataCore pars.parallelizer n ))));
          })
        ];
      } // (if hasAttr "shape" pars then { shape = pars.shape; } else {})
        // (if hasAttr "color" pars then { color = pars.color; } else {}))
    ) (next.parameters // {
        label = "plot";
        xs = (range 1 120);
    });


    multiplot = next.callExperiment (pars:
      MultiPlot {
        inherit (pars) name title xlab ylab xlog ylog xlim ylim forceRerun;
        plots =
          map (arg: (next.plot.override (pars // {
                 label = pars.mkLabel arg;
                } // (if hasAttr "mkShape" pars then { shape = pars.mkShape arg; } else {})
                  // (if hasAttr "mkColor" pars then { color = pars.mkColor arg; } else {})
                  // arg)))
              (cartesianProductOfSets pars.labels);
      }
    ) (next.parameters // rec {
        name = "time-vs-ncore";
        title = "SleepTime vs Ncore for Workload using conduit";
        model = "Sleep";
        xlab = "Ncore";
        ylab = "Seconds";
        xlog = false;
        ylog = true;
        xlim = null;
        ylim = null;
        xs = (range 1 60);
        labels = {
          host = with hosts; [ hserver7 node04 ];
          workLoad = [ 100 1000 10000 100000 1000000 ];
        };
        mkLabel = { workLoad, host }: "${host.name}-${toString workLoad}";
    });

    tileplot = next.callExperiment (pars:
      let layout = if hasAttr "layout" pars
            then pars.layout
            else (let len = length pars.plots; in if len <= 4 then "${toString len}x" else "4x");
      in
      TilePlot {
        inherit layout;
        inherit (pars) plots forceRerun;
      }
    ) (next.parameters // rec {
        plots = [ next.plot next.multiplot ];
    });

    color = {

      # parallelizer
      ConduitMVar = "red";
      ConduitTMVar = "blue";
      ConduitQSem = "magenta";
      ConduitCapPool = "brown";
      ConduitCapPool2 = "purple";
      ConcurrentlyOn = "teal";
      Concurrently = "green";
      AsyncBound = "steelblue";
      ParList = "gold";
      ParMap = "cyan";

      # node
      hserver7 = "red";
      node01 = "blue";
      node02 = "green";
      node03 = "gold";
      node04 = "cyan";
      node05 = "magenta";

      # heapSize
      "4m" = "red";
      "8m" = "blue";
      "16m" = "green";
      "32m" = "gold";
      "64m" = "cyan";
      "128m" = "magenta";
      "256m" = "purple";
      "512m" = "brown";

      # workloads
      "10" = "red";
      "100" = "blue";
      "1000" = "green";
      "10000" = "gold";
      "100000" = "cyan";
      "1000000" = "magenta";
      "10000000" = "purple";
      "100000000" = "brown";

      # computation models
      Sleep = "gold";
      Ponder = "green";
      Reduce = "red";
      ReduceCompact = "blue";
      NormVector = "magenta";
      NormFFIUnsafe = "purple";
      NormFFISafe = "brown";
      NormBlas = "cyan";

      # computation models
      "8107" = "gold";
      "902" = "green";
      "924" = "red";
      "944" = "blue";

      "nollvm" = "green";
      "llvm11" = "magenta";
      "llvm12" = "red";
      "llvm13" = "blue";
      "llvm14" = "gold";

    };


    shape = {

      # parallelizer
      ConduitMVar = "circle";
      ConduitTMVar = "poly3d";
      ConduitQSem = "poly5u";
      ConduitCapPool = "poly5d";
      ConduitCapPool2 = "poly4u";
      ConcurrentlyOn = "poly6u";
      Concurrently = "cross";
      AsyncBound = "poly6d";
      ParList = "stars";
      ParMap = "poly3u";

      # node
      hserver7 = "circle";
      node01 = "cross";
      node02 = "stars";
      node03 = "poly3u";
      node04 = "poly4u";
      node05 = "poly5u";

      # workload
      "10" = "poly3d";
      "100" = "circle";
      "1000" = "cross";
      "10000" = "stars";
      "100000" = "poly3u";
      "1000000" = "poly4u";
      "10000000" = "poly5u";
      "100000000" = "poly6u";

      # computation models
      Sleep = "stars";
      Ponder = "ploly3u";
      Reduce = "circle";
      ReduceCompact = "cross";
      NormVector = "ploy5u";
      NormFFIUnsafe = "poly6u";
      NormFFISafe = "poly3d";
      NormBlas = "poly5d";

    };

    availableColors = ["blue" "red" "green" "gold" "cyan" "magenta" "purple" "brown" "steelblue" "teal" ];
    availableShapes = [ "circle" "cross" "stars" "poly3u" "poly3d" "poly4u" "poly4d" "poly5u" "poly5d" "poly6u" "poly6d" ];

  });

  multiplots = default.extend (next: prev: {


    defaultModels = [
        "Sleep"          # no computation at all
        "Ponder"         # arithmetics without memory requirement
        # "Reduce"         # simple arithmetic but with heavy memory requirement
        # "ReduceCompact"  # same as Reduce but the memory assigned in the compact region
        # "NormVector"     # sample random vector and compute norm
        # "NormFFIUnsafe"  # compute norm using a custom C function using unsafe FFI call
        # "NormFFISafe"    # compute norm using a custom C function using safe FFI call
        # "NormBlas"       # compute norm using a library function in hmatrix (blas)
        # "DotFFIUnsafe"   # compute inner product using a custom C function using unsafe FFI call
        # "DotFFISafe"     # compute inner product using a custom C function using safe FFI call
        # "DotBlas"        # compute inner product using a library function in hmatrix (blas)
        # "DotFFIUnsafe2"  # same as DotFFIUnsafe but include vector allocation
        # "DotFFISafe2"    # same as DotFFISafe but include vector allocation
        # "DotBlas2"       # same as DotBlas but include vector allocation
      ];
    defaultParallelizers = [
        # "ConduitMVar"
        # "ConduitTMVar" # terrible parallelization overhead
        # "ConduitQSem"
        # "ConduitCapPool"
        # "ConduitCapPool2" # main threads does not perform the computation
        "ConcurrentlyOn"  # use forkOn interally instead of forkIO
        "Concurrently"
        # "Async"
        "AsyncBound"  # completely searialize
        # "ParList"
        # "ParMap"
      ];
    defaultWorkLoads = [ 100 1000 ];

    #-----------------------------------------
    #
    #   time vs ncore for various workload
    #

    plot-workload-list = concatLists (
      flip map next.defaultModels (model:
        (flip map next.defaultParallelizers (parallelizer:
          {
            name = "${model}-${parallelizer}";
            value = next.multiplot.override {
              title = "${model} for Various Workloads using ${parallelizer}";
              inherit model parallelizer;
              xs = (range 1 120);
              ylim = "1e-4 1e+1";
              labels = {
                workLoad = next.defaultWorkLoads;
              };
              # forceRerun = true;
              mkLabel = { workLoad }: "${toString workLoad}";
              mkShape = { workLoad }: next.shape."${toString workLoad}";
              mkColor = { workLoad }: next.color."${toString workLoad}";
            };
          }))
      ));

    plot-workload = __listToAttrs next.plot-workload-list;

    plot-tile-workload =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-workload-list;
      };


    #-----------------------------------------
    #
    #   time vs ncore for various workload as well as -qa option on/off
    #

    plot-workload-qa-list = concatLists (
      flip map next.defaultModels (model:
        (flip map next.defaultParallelizers (parallelizer:
          {
            name = "${model}-${parallelizer}";
            value = next.multiplot.override {
              title = "${model} for Various Workloads using ${parallelizer}";
              inherit model parallelizer;
              xs = (range 1 120);
              ylim = "1e-4 1e+1";
              labels = {
                qa = [ true false ];
                workLoad = next.defaultWorkLoads;
              };
              # forceRerun = true;
              mkLabel = { qa, workLoad }: "${toString workLoad}-${if qa then "qa" else "noqa"}";
              mkShape = { qa, workLoad }: next.shape."${toString workLoad}";
              mkColor = { qa, workLoad }: "${if qa then "red" else "blue"}";
            };
          }))
      ));

    plot-workload-qa = __listToAttrs next.plot-workload-qa-list;

    plot-tile-workload-qa =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-workload-qa-list;
      };


    #-----------------------------------------------------------
    #
    #   time vs ncore for various workloads and parallelizers
    #

    plot-workload-parallelizer-list = (
      flip map next.defaultModels (model:
          {
            name = "${model}";
            value = next.multiplot.override {
              title = "${model} for Various Workloads";
              inherit model;
              xs = (range 1 120);
              ylim = "1e-4 1e+1";
              labels = {
                workLoad = next.defaultWorkLoads;
                parallelizer = next.defaultParallelizers;
              };
              mkLabel = { workLoad, parallelizer }: "${parallelizer}-${toString workLoad}";
              mkShape = { workLoad, parallelizer }: next.shape."${toString workLoad}";
              mkColor = { workLoad, parallelizer }: next.color."${parallelizer}";
            };
          }));

    plot-workload-parallelizer = __listToAttrs next.plot-workload-parallelizer-list;

    plot-tile-workload-parallelizer =
      next.tileplot.override {
        layout = "3x";
        plots = map (s: s.value) next.plot-workload-parallelizer-list;
      };


    #-----------------------------------------------------------
    #
    #   time vs ncore for various workloads and hosts
    #

    plot-workload-host-list = concatLists (
      flip map next.defaultModels (model:
        (flip map next.defaultParallelizers (parallelizer:
          {
            name = "${model}-${parallelizer}";
            value =next.multiplot.override {
              title = "${model} for Various Workloads using ${parallelizer}";
              inherit model parallelizer;
              xs = (range 1 60);
              ylim = "1e-4 1e+1";
              labels = {
                host = with hosts; [ hserver7 node03 node04];
                workLoad = next.defaultWorkLoads;
              };
              mkLabel = { host, workLoad }: "${host.name}-${toString workLoad}";
              mkShape = { host, workLoad }: next.shape."${host.name}";
              mkColor = { host, workLoad }: next.color."${toString workLoad}";
            };
          }))
      ));

    plot-workload-host = __listToAttrs next.plot-workload-host-list;

    plot-tile-workload-host =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-workload-host-list;
      };


    #-----------------------------------------------------------
    #
    #   time vs ncore for various workloads and (reduce or reduceCompact)
    #   to see the effect of the compact memory region
    #

    plot-compact-list = concatLists (
      flip map next.defaultWorkLoads (workLoad:
        (flip map next.defaultParallelizers (parallelizer:
          {
            name = "${toString workLoad}-${parallelizer}";
            value =next.multiplot.override {
              title = "listSize=${toString (50 * workLoad)}, Parallelizer=${parallelizer}";
              inherit workLoad parallelizer;
              xs = (range 1 10);
              ylim = "1e-4 1e+1";
              labels = {
                model = [ "Reduce" "ReduceCompact" ];
              };
              mkLabel = { model }: "${model}";
              mkShape = { model }: next.shape."${model}";
              mkColor = { model }: next.color."${model}";
            };
          }))
      ));

    plot-compact = __listToAttrs next.plot-compact-list;

    plot-tile-compact =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-compact-list;
      };



    #-----------------------------------------------------------
    #
    #   heapSize experiments
    #

    plot-heapsize-list = concatLists (concatLists (
      flip map next.defaultModels (model:
        (flip map [ "4m" "16m" "64m" "256m" ] (heapSize:
          (flip map next.defaultParallelizers (parallelizer:
            rec {
              name = "${model}-${heapSize}-${parallelizer}";
              value = next.multiplot.override {
                title = name;
                inherit model heapSize parallelizer;
                xs = (range 1 120);
                ylim = "1e-4 1e+1";
                host = next.hosts.node03;
                labels = {
                  workLoad = next.defaultWorkLoads;
                };
                mkLabel = { workLoad }: "${toString workLoad}";
                mkShape = { workLoad }: next.shape."${toString workLoad}";
                mkColor = { workLoad }: next.color."${toString workLoad}";
              };
            })))))));

    plot-heapsize = __listToAttrs next.plot-heapsize-list;

    plot-tile-heapsize =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-heapsize-list;
      };


    #-----------------------------------------------------------
    #
    #   llvm experiments
    #

    plot-llvm-list = concatLists
      (flip map next.defaultModels (model:
        (flip map next.defaultParallelizers (parallelizer:
          rec {
            name = "${model}-${parallelizer}";
            value = next.multiplot.override {
              title = name;
              inherit model parallelizer;
              xs = (range 1 126);
              ylim = "1e-4 1e+1";
              host = next.hosts.node03;
              labels = {
                workLoad = next.defaultWorkLoads;
                llvm = [ "nollvm" "llvm11" "llvm12" "llvm14" ];
              };
              mkLabel = { llvm, workLoad }: "${llvm}-${toString workLoad}";
              mkShape = { llvm, workLoad }: next.shape."${toString workLoad}";
              mkColor = { llvm, workLoad }: next.color."${llvm}";
            };
          }))));

    plot-llvm = __listToAttrs next.plot-llvm-list;

    plot-tile-llvm =
      next.tileplot.override {
        layout = "${toString (length next.defaultParallelizers)}x";
        plots = map (s: s.value) next.plot-llvm-list;
      };

  });


in
{
  inherit default;
} // (filterAttrs (n: v: hasPrefix "plot-" n) multiplots)
