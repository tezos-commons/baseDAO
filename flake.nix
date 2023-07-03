# SPDX-FileCopyrightText: 2023 Tezos Commons
# SPDX-License-Identifier: LicenseRef-MIT-TC

{
  description = "The baseDAO flake";

  nixConfig.flake-registry = "https://gitlab.com/morley-framework/morley-infra/-/raw/main/flake-registry.json";

  inputs = {
    morley-infra.url = "gitlab:morley-framework/morley-infra";
    nix-npm-buildpackage.url = "github:serokell/nix-npm-buildpackage";
    morley.url = "gitlab:morley-framework/morley";
  };

  outputs = { self, flake-utils, morley-infra, morley, nix-npm-buildpackage, ... }:
    (flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        nixpkgs = morley-infra.legacyPackages.${system};

        pkgs = nixpkgs.extend nix-npm-buildpackage.overlays.default;

        ligo = pkgs.stdenv.mkDerivation {
          name = "ligo";
          src = pkgs.fetchurl {
            url = "https://gitlab.com/ligolang/ligo/-/jobs/2684632179/artifacts/raw/ligo";
            sha256 = "sha256-sr1gxTDB99F3Cmm2tl2280MAOCT4ONtW9Xv69NdOqIk=";
          };
          dontUnpack = true;
          installPhase = ''
            mkdir -p $out/bin
            cp $src $out/bin/ligo;
            chmod +x $out/bin/ligo
          '';
        };

        inherit (morley-infra.utils.${system}) weeder-hacks;

        # all local packages and their subdirectories
        # we need to know subdirectories to make weeder stuff work
        local-packages = [
          { name = "baseDAO-ligo-meta"; subdirectory = "./haskell"; }
        ];

        # names of all local packages
        local-packages-names = map (p: p.name) local-packages;

        projectSrc = pkgs.haskell-nix.haskellLib.cleanGit {
          name = "baseDAO";
          src = ./.;
        };

        # haskell.nix package set
        # parameters:
        # - release – 'true' for "release" (e. g. master) build,
        #   'false' for "development" (e. g. PR) build.
        # - commitSha, commitDate – git revision info used for contract documentation.
        hs-pkgs = { release, commitSha ? null, commitDate ? null }:
          pkgs.haskell-nix.stackProject {
            src = projectSrc;

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
                    cp ${self.packages.${system}.build-ligo}/* ./resources
                  '';
                };
              }
            ];

            shell = {
              tools = {
                cabal = {};
                hlint = { version = "3.4"; };
                hpack = { version = "0.34.4"; };
              };
            };
        };

        hs-pkgs-development = hs-pkgs { release = false; };

        flake = hs-pkgs-development.flake {};

        # component set for all local packages like this:
        # { baseDAO = { library = ...; exes = {...}; tests = {...}; ... };
        #   ...
        # }
        packages = pkgs.lib.genAttrs local-packages-names (packageName: hs-pkgs-development."${packageName}".components);

      in pkgs.lib.lists.foldr pkgs.lib.recursiveUpdate {} [

        { inherit (flake) packages apps; }

        {
          legacyPackages = pkgs.extend (final: prev: {
            morley = morley.packages.${system}."morley:exe:morley";
            tezos-client = morley-infra.legacyPackages.${system}.octez-client;
          });

          devShells.default = (flake).devShell;

          packages = {
            # a derivation which generates a script for running weeder
            weeder-script = morley-infra.utils.${system}.weeder-script {
              hs-pkgs = hs-pkgs-development;
              inherit local-packages;
            };

            all-components = pkgs.linkFarmFromDrvs "all-components"
              (builtins.attrValues (flake).packages);

            default = self.packages.${system}.all-components;

            build-ligo = pkgs.stdenv.mkDerivation {
              name = "baseDAO-ligo";
              src = ./.;
              nativeBuildInputs = [ ligo ];
              buildPhase = "make all; make test-storage";
              installPhase = "mkdir -p $out; cp -r out/* $out";
            };

            build-typescript = pkgs.buildYarnPackage {
              src = ./typescript/baseDAO;
              yarnBuild = ''
                yarn install
                yarn tsc
              '';
            };
          };

          checks = {
            trailing-whitespace = pkgs.build.checkTrailingWhitespace ./.;

            reuse-lint = pkgs.build.reuseLint ./.;
          };

          utils = {
            inherit (morley-infra.utils.${system}) run-chain-tests;

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
          };
        }
      ]));
}
