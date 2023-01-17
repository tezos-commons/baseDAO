# SPDX-FileCopyrightText: 2021 Tezos Commons
# SPDX-License-Identifier: LicenseRef-MIT-TC

# This file is derived from
# https://gitlab.com/morley-framework/morley/-/blob/99426dc89cf8c03eaeae4d62cbe67a0c008b60fc/ci.nix
# and thus is more complicated than necessary (supports a list of packages).
# Currently we don't care about that.

rec {
  sources = import ./nix/sources.nix;
  haskell-nix = pkgs.haskell-nix;
  pkgs = morley-infra.legacyPackages.${builtins.currentSystem};
  ligo = (pkgs.runCommand "ligo" {} "mkdir -p $out/bin; cp ${sources.ligo} $out/bin/ligo; chmod +x $out/bin/ligo");
  morley = (import "${sources.morley}/ci.nix").packages.morley.exes.morley;
  inherit (pkgs.callPackage sources.nix-npm-buildpackage { }) buildYarnPackage;
  morley-infra = import sources.morley-infra;

  inherit (morley-infra) stack2cabal weeder-hacks run-chain-tests;

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
    src = pkgs.haskell-nix.haskellLib.cleanGit { name = "baseDAO"; src = ./.; };

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
            mkdir -p ./resources
            cp ${build-ligo}/* ./resources
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

  # a derivation which generates a script for running weeder
  weeder-script = morley-infra.weeder-script {
    hs-pkgs = hs-pkgs-development;
    inherit local-packages;
  };
}
