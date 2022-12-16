# SPDX-FileCopyrightText: 2022 TQ Tezos
# SPDX-License-Identifier: LicenseRef-MIT-TQ

let
  inherit (import
  (let lock = builtins.fromJSON (builtins.readFile ./flake.lock); in
    fetchTarball {
      url = "https://github.com/edolstra/flake-compat/archive/${lock.nodes.flake-compat.locked.rev}.tar.gz";
      sha256 = lock.nodes.flake-compat.locked.narHash;
    })
  { src = ./.; }) defaultNix;
in defaultNix //
   (defaultNix.legacyPackages.${__currentSystem}.lib.attrsets.mapAttrs (_: val: val.${__currentSystem}) defaultNix)
