# SPDX-FileCopyrightText: 2021 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

# This file is derived from
# https://gitlab.com/morley-framework/morley/-/blob/99426dc89cf8c03eaeae4d62cbe67a0c008b60fc/ci.nix
# and thus is more complicated than necessary (supports a list of packages).
# Currently we don't care about that.

rec {
  sources = import ./nix/sources.nix;
  xrefcheck = import sources.xrefcheck;
  haskell-nix = import sources."haskell.nix" {
    sourcesOverride = { hackage = sources."hackage.nix"; stackage = sources."stackage.nix"; };
  };
  pkgs = import sources.nixpkgs haskell-nix.nixpkgsArgs;
  weeder-hacks = import sources.haskell-nix-weeder { inherit pkgs; };
  tezos-client = (import "${sources.tezos-packaging}/nix/build/pkgs.nix" {}).ocamlPackages.tezos-client;
  ligo = (pkgs.runCommand "ligo" {} "mkdir -p $out/bin; cp ${sources.ligo} $out/bin/ligo; chmod +x $out/bin/ligo");
  morley = (import "${sources.morley}/ci.nix").packages.morley.exes.morley;
  inherit (pkgs.callPackage sources.nix-npm-buildpackage { }) buildYarnPackage;

  # all local packages and their subdirectories
  # we need to know subdirectories to make weeder stuff work
  local-packages = [
    { name = "baseDAO-ligo-meta"; subdirectory = "./haskell"; }
  ];

  # names of all local packages
  local-packages-names = map (p: p.name) local-packages;

  # haskell.nix package set
  # parameters:
  # - release – 'true' for "release" (e. g. master) build,
  #   'false' for "development" (e. g. PR) build.
  # - commitSha, commitDate – git revision info used for contract documentation.
  hs-pkgs = { release, commitSha ? null, commitDate ? null }: pkgs.haskell-nix.stackProject {
    src = pkgs.haskell-nix.haskellLib.cleanGit { src = ./.; };

    modules = [
      {
        # common options for all local packages:
        packages = pkgs.lib.genAttrs local-packages-names (packageName: {
          ghcOptions = with pkgs.lib; (
            ["-O0" "-Werror"]
            # produce *.dump-hi files, required for weeder:
            ++ optionals (!release) ["-ddump-to-file" "-ddump-hi"]
          );

          # enable haddock for local packages
          doHaddock = true;

          # in non-release mode collect all *.dump-hi files (required for weeder)
          postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
        });

        # disable haddock for dependencies
        doHaddock = false;
      }

      # provide commit sha and date for tezos-nbit in release mode:
      {
        packages.baseDAO = {
          preBuild = ''
            export MORLEY_DOC_GIT_COMMIT_SHA=${if release then pkgs.lib.escapeShellArg commitSha else "UNSPECIFIED"}
            export MORLEY_DOC_GIT_COMMIT_DATE=${if release then pkgs.lib.escapeShellArg commitDate else "UNSPECIFIED"}
          '';
        };
        packages.baseDAO-ligo-meta = {
          preBuild = ''
            mkdir -p ./haskell/test
            cp -r ${build-ligo}/* ./haskell/test
          '';
        };
      }
    ];
  };

  hs-pkgs-development = hs-pkgs { release = false; };

  # component set for all local packages like this:
  # { baseDAO = { library = ...; exes = {...}; tests = {...}; ... };
  #   ...
  # }
  packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

  # returns a list of all components (library + exes + tests + benchmarks) for a package
  get-package-components = pkg: with pkgs.lib;
    optional (pkg ? library) pkg.library
    ++ attrValues pkg.exes
    ++ attrValues pkg.tests
    ++ attrValues pkg.benchmarks;

  # per-package list of components
  components = pkgs.lib.mapAttrs (pkgName: pkg: get-package-components pkg) packages;

  # a list of all components from all packages in the project
  all-components = with pkgs.lib; flatten (attrValues components);

  # build haddock
  haddock = with pkgs.lib; flatten (attrValues
    (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages));

  # run baseDAO to produce contract documents
  contracts-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs:
    pkgs.runCommand "contracts-doc" {
      buildInputs = [ (hs-pkgs releaseArgs).baseDAO.components.exes.baseDAO ];
    } ''
      mkdir $out
      cd $out
      baseDAO document --name TrivialDAO --output TrivialDAO.md
      baseDAO document --name RegistryDAO --output RegistryDAO.md
      baseDAO document --name TreasuryDAO --output TreasuryDAO.md
      baseDAO document --name GameDAO --output GameDAO.md
    '';

  build-ligo = pkgs.stdenv.mkDerivation {
    name = "baseDAO-ligo";
    src = ./.;
    nativeBuildInputs = [ ligo ];
    buildPhase = "make all; make test-storage";
    installPhase = "mkdir -p $out; cp -r out/* $out";
  };

  build-typescript = buildYarnPackage {
    src = ./typescript/baseDAO;
    yarnBuild = ''
      yarn install
      yarn tsc
    '';
  };

  # nixpkgs has weeder 2, but we use weeder 1
  weeder-legacy = pkgs.haskellPackages.callHackageDirect {
    pkg = "weeder";
    ver = "1.0.9";
    sha256 = "0gfvhw7n8g2274k74g8gnv1y19alr1yig618capiyaix6i9wnmpa";
  } {};

  # a derivation which generates a script for running weeder
  weeder-script = weeder-hacks.weeder-script {
    weeder = weeder-legacy;
    hs-pkgs = hs-pkgs-development;
    local-packages = local-packages;
  };

  # stack2cabal in nixpkgs is broken because fixes for ghc 8.10 have not
  # been released to hackage yet, take sources from github
  stack2cabal = pkgs.haskellPackages.stack2cabal.overrideAttrs (o: {
    src = pkgs.fetchFromGitHub {
      owner = "hasufell";
      repo = "stack2cabal";
      rev = "afa113beb77569ff21f03fade6ce39edc109598d";
      sha256 = "1zwg1xkqxn5b9mmqafg87rmgln47zsmpgdkly165xdzg38smhmng";
    };
  });
}
