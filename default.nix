let
  localLib = import ./lib.nix;
in
{ system ? builtins.currentSystem
, config ? {}
, gitrev ? "unknown"
, pkgs ? (import (localLib.fetchNixPkgs) { inherit system config; })
# profiling slows down performance by 50% so we don't enable it by default
, enableProfiling ? false
}:

with pkgs.lib;
with pkgs.haskell.lib;

let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  cardanoPkgs = ((import ./pkgs { inherit pkgs; }).override {
    overrides = self: super: {
      cardano-sl = overrideCabal super.cardano-sl (drv: {
        # production full nodes shouldn't use wallet as it means different constants
        configureFlags = [
          "-f-asserts"
          "-f-dev-mode"
        ];
        testTarget = "--log=test.log || (sleep 10 && kill $TAILPID && false)";
        preCheck = ''
          mkdir -p dist/test
          touch dist/test/test.log
          tail -F dist/test/test.log &
          export TAILPID=$!
        '';
        postCheck = ''
          sleep 10
          kill $TAILPID
        '';
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
        enableExecutableProfiling = enableProfiling;
        passthru = {
          inherit enableProfiling;
        };
      });
      cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
        configureFlags = [
          "-f-asserts"
          "-f-dev-mode"
          "--ghc-options=-DGITREV=${gitrev}"
        ];
      });

      cardano-sl-wallet = justStaticExecutables super.cardano-sl-wallet;
      cardano-sl-tools = justStaticExecutables (overrideCabal super.cardano-sl-tools (drv: {
        # waiting on load-command size fix in dyld
        doCheck = ! pkgs.stdenv.isDarwin;
      }));

      cardano-sl-static = justStaticExecutables self.cardano-sl;
      cardano-sl-explorer-static = justStaticExecutables self.cardano-sl-explorer;
      cardano-report-server-static = justStaticExecutables self.cardano-report-server;

      # Undo configuration-nix.nix change to hardcode security binary on darwin
      # This is needed for macOS binary not to fail during update system (using http-client-tls)
      # Instead, now the binary is just looked up in $PATH as it should be installed on any macOS
      x509-system = overrideDerivation super.x509-system (drv: {
        postPatch = ":";
      });

      # Gold linker fixes
      cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;

      # Darwin fixes upstreamed in nixpkgs commit 71bebd52547f4486816fd320bb3dc6314f139e67
      hinotify = if pkgs.stdenv.isDarwin then self.hfsevents else super.hinotify;
      hfsevents = self.callPackage ./pkgs/hfsevents.nix { inherit (pkgs.darwin.apple_sdk.frameworks) Cocoa CoreServices; };
      fsnotify = if pkgs.stdenv.isDarwin
        then addBuildDepend (dontCheck super.fsnotify) pkgs.darwin.apple_sdk.frameworks.Cocoa
        else dontCheck super.fsnotify;

      mkDerivation = args: super.mkDerivation (args // {
        enableLibraryProfiling = enableProfiling;
      });
    };
  });
  dockerImage = let
    DOMAIN = "aws.iohk.io";
    topologyFile = pkgs.writeText "topology.yaml" ''
      wallet:
        relays:
          [
            [
              { host: cardano-node-0.${DOMAIN}, port: 3000 },
              { host: cardano-node-1.${DOMAIN}, port: 3000 },
              { host: cardano-node-2.${DOMAIN}, port: 3000 },
              { host: cardano-node-3.${DOMAIN}, port: 3000 },
              { host: cardano-node-4.${DOMAIN}, port: 3000 },
              { host: cardano-node-5.${DOMAIN}, port: 3000 },
              { host: cardano-node-6.${DOMAIN}, port: 3000 }
            ]
          ]
        valency: 1
        fallbacks: 7
    '';
    CLUSTER = "testnet-1.0";
    SYSTEM_START_TIME = 1504820421;
  in pkgs.dockerTools.buildImage {
    name = "cardano-container-${CLUSTER}";
    contents = cardanoPkgs.cardano-sl-static;
    config = {
      Cmd = [
        "${cardanoPkgs.cardano-sl-wallet}/bin/cardano-node"
        "--tlscert" "${cardanoPkgs.cardano-sl.src}/../scripts/tls-files/server.crt"
        "--tlskey" "${cardanoPkgs.cardano-sl.src}/../scripts/tls-files/server.key"
        "--tlsca" "${cardanoPkgs.cardano-sl.src}/../scripts/tls-files/ca.crt"
        "--no-ntp"
        "--topology" "${topologyFile}"
        "--log-config" "${cardanoPkgs.cardano-sl.src}/../scripts/log-config-qa.yaml"
        "--logs-prefix" "logs/${CLUSTER}"
        "--db-path" "db-${CLUSTER}"
        "--wallet-db-path" "wdb-${CLUSTER}"
        "--configuration-file" "${cardanoPkgs.cardano-sl.src}/configuration.yaml"
        "--configuration-key" "mainnet_wallet_macos64"
        "--system-start" (toString SYSTEM_START_TIME)
      ];
      ExposedPorts = {
        "3000/tcp" = {};
      };
    };
  };
  upstream = {
    inherit dockerImage;
    stack2nix = import (pkgs.fetchFromGitHub {
      owner = "input-output-hk";
      repo = "stack2nix";
      rev = "be52e67113332280911bcc4924d42f90e21f1144";
      sha256 = "13n7gjyzll3prvdsb6kjyxk9g0by5bv0q34ld7a2nbvdcl1q67fb";
    }) { inherit pkgs; };
    inherit (pkgs) purescript;
  };
in cardanoPkgs // upstream
