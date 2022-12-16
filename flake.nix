# SPDX-FileCopyrightText: 2021 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

{
  description = "The baseDAO flake";

  nixConfig.flake-registry = "https://gitlab.com/morley-framework/morley-infra/-/raw/main/flake-registry.json";

  inputs.morley-infra.url = "gitlab:morley-framework/morley-infra";

  outputs = { self, haskell-nix, flake-utils, morley-infra, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        pkgs = morley-infra.legacyPackages.${system};

        inherit (morley-infra) weeder-hacks;

        # all local packages and their subdirectories
        # we need to know subdirectories for weeder and for cabal check
        local-packages = [
          { name = "baseDAO-ligo-meta"; subdirectory = "./haskell"; }
        ];

        # names of all local packages
        local-packages-names = map (p: p.name) local-packages;

        # source with gitignored files filtered out
        projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "baseDAO";
          src = ./.;
        };

        # haskell.nix package set
        # parameters:
        # - release -- 'true' for master and producion branches builds, 'false' for all other builds.
        #   This flag basically disables weeder related files production, haddock and enables stripping
        # - commitSha, commitDate -- git revision info used during possible autodoc build
        # - optimize -- 'true' to enable '-O1' ghc flag, we intend to use it only in production branch
        hs-pkgs = { release, commitSha ? null, commitDate ? null }: pkgs.haskell-nix.stackProject {
          src = projectSrc;

          # use .cabal files for building because:
          # 1. haskell.nix fails to work for package.yaml with includes from the parent directory
          # 2. with .cabal files haskell.nix filters out source files for each component, so only the changed components will rebuild
          ignorePackageYaml = true;

          modules = [
            # common options for all local packages:
            {
              packages = pkgs.lib.genAttrs local-packages-names (packageName: {
                ghcOptions = with pkgs.lib; (
                  # disable optimizations, fail on warnings
                  [ "-O0" "-Werror" ]
                  # produce *.dump-hi files, required for weeder:
                  ++ optionals (!release) ["-ddump-to-file" "-ddump-hi"]
                );

                # enable haddock for local packages
                doHaddock = true;

                # in non-release mode collect all *.dump-hi files (required for weeder)
                postInstall = if release then null else weeder-hacks.collect-dump-hi-files;
              });
            }

            {
              # don't haddock dependencies
              doHaddock = false;

              # provide commit sha and date for baseDAO in release mode:
              packages.baseDAO = {
                preBuild = ''
                  export MORLEY_DOC_GIT_COMMIT_SHA=${if release then pkgs.lib.escapeShellArg commitSha else "UNSPECIFIED"}
                  export MORLEY_DOC_GIT_COMMIT_DATE=${if release then pkgs.lib.escapeShellArg commitDate else "UNSPECIFIED"}
                '';
              };

            packages.baseDAO-ligo-meta =
              let
                ligo = (pkgs.runCommand "ligo" {} "mkdir -p $out/bin; cp ${pkgs.ligo} $out/bin/ligo; chmod +x $out/bin/ligo");
                build-ligo = pkgs.stdenv.mkDerivation {
                  name = "baseDAO-ligo";
                  src = ./.;
                  nativeBuildInputs = [ ligo ];
                  buildPhase = "make all; make test-storage";
                  installPhase = "mkdir -p $out; cp -r out/* $out";
                };
              in {
                preBuild = ''
                   mkdir -p ./resources
                   cp ${build-ligo}/* ./resources
                '';
              };
            }
          ];

          shell = {
            tools = {
              cabal = {};
              hlint = { version = "3.4"; };
              hpack = { version = "0.34.4"; };
              haskell-language-server = {};
            };
            buildInputs = with pkgs; [
              haskellPackages.implicit-hie
            ];
          };
        };

        hs-pkgs-development = hs-pkgs { release = false; };

        # component set for all local packages like this:
        # { morley = { library = ...; exes = {...}; tests = {...}; ... };
        #   morley-prelude = { ... };
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

        flake = hs-pkgs-development.flake {};

      in pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [
        {
          inherit (morley-infra) run-chain-tests;


          contracts-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs: pkgs.runCommand "contracts-doc" {
            buildInputs = [
              (hs-pkgs releaseArgs).baseDAO.components.exes.baseDAO
            ];
          } ''
            mkdir $out
            cd $out
            mkdir autodoc
            baseDAO document --name GenericWithCustomErrors --output \
              autodoc/GenericWithCustomErrors.md
          '';
        }

        {
          legacyPackages = pkgs;

          packages = {
/*
            # run baseDAO to produce contract documents
            contracts-doc = { release, commitSha ? null, commitDate ? null }@releaseArgs:
              pkgs.runCommand "contracts-doc" {
                buildInputs = [ (hs-pkgs releaseArgs).baseDAO-ligo-meta.components.exes.baseDAO ];
              } ''
                mkdir $out
                cd $out
                baseDAO document --name TrivialDAO --output TrivialDAO.md
                baseDAO document --name RegistryDAO --output RegistryDAO.md
                baseDAO document --name TreasuryDAO --output TreasuryDAO.md
                baseDAO document --name GameDAO --output GameDAO.md
              '';
*/
            # a derivation which generates a script for running weeder
            weeder-script = morley-infra.weeder-script {
              hs-pkgs = hs-pkgs-development;
              inherit local-packages;
            };

            haddock = with pkgs.lib; pkgs.linkFarmFromDrvs "haddock" (flatten (attrValues
              (mapAttrs (pkgName: pkg: optional (pkg ? library) pkg.library.haddock) packages)));

            all-components = pkgs.linkFarmFromDrvs "all-components" (builtins.attrValues flake.packages);

            default = self.packages.${system}.all-components;
          };

          checks = {
            # checks if all packages are appropriate for uploading to hackage
            run-cabal-check = morley-infra.run-cabal-check { inherit local-packages projectSrc; };

            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;

            reuse-lint = pkgs.build.reuseLint ./.;
          };
        }

        { inherit (flake) packages devShells apps; }
      ]));
}
