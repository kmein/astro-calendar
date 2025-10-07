{
  description = "Generate calendar files for astrological events";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    almanac.url = "github:lfborjas/almanac";
    almanac.flake = false;
    swiss-ephemeris.url = "github:lfborjas/swiss-ephemeris";
    swiss-ephemeris.flake = false;
    swisseph.url = "github:aloistr/swisseph";
    swisseph.flake = false;
  };

  outputs =
    inputs:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = inputs.nixpkgs.lib.genAttrs supportedSystems;
      pkgsForSystem =
        system:
        import inputs.nixpkgs {
          inherit system;
          overlays = [
            inputs.self.overlays.default
          ];
        };
    in
    {
      overlays.default =
        final: prev:
        let
          hsLib = prev.haskell.lib;
        in
        {
          haskellPackages = prev.haskellPackages.extend (
            finalHaskell: prevHaskell: {
              swiss-ephemeris = hsLib.dontCheck (
                hsLib.doJailbreak (prevHaskell.callCabal2nix "swiss-ephemeris" inputs.swiss-ephemeris { })
              );
              almanac = hsLib.doJailbreak (
                prevHaskell.callCabal2nix "almanac" inputs.almanac {
                  inherit (finalHaskell) swiss-ephemeris;
                }
              );
              iCalendar = hsLib.unmarkBroken (hsLib.doJailbreak prevHaskell.iCalendar);
              astro-calendar = hsLib.doHaddock (
                prevHaskell.callCabal2nix "astro-calendar" ./. {
                  inherit (finalHaskell) almanac swiss-ephemeris iCalendar;
                }
              );
            }
          );
          swissEphemeris = prev.runCommand "ephemeris" {} ''
            mkdir -p $out
            cp -rf ${inputs.swisseph.outPath}/ephe/* $out
            cp -rf ${inputs.almanac.outPath}/test/ephe/* $out
          '';
        };

      packages = forAllSystems (system: let pkgs = pkgsForSystem system; in {
        default = pkgs.haskellPackages.astro-calendar;
        swissEphemeris = pkgs.swissEphemeris;
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsForSystem system;
        in
        {
          default = pkgs.haskellPackages.shellFor {
            packages = ps: [
              ps.astro-calendar
              # if those are added here, they do not appear in the hoogle and haddock
              # ps.almanac
              # ps.swiss-ephemeris
            ];
            withHoogle = true;
            nativeBuildInputs = [
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.hlint
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.cabal-install
              (pkgs.python3.withPackages (py: [
                py.pandas
                py.matplotlib
                py.networkx
                py.reportlab
              ]))
            ];
            exactDeps = true;
            shellHook = ''
              export SE_EPHE_PATH=${inputs.self.packages.${system}.swissEphemeris}
            '';
          };
        }
      );
    };
}
