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
      overlays.default = final: prev: {
        haskellPackages = prev.haskellPackages.extend (
          finalHaskell: prevHaskell: {
            swiss-ephemeris = prev.haskell.lib.doJailbreak (
              prevHaskell.callCabal2nix "swiss-ephemeris" inputs.swiss-ephemeris { }
            );
            almanac = prev.haskell.lib.doJailbreak (
              prevHaskell.callCabal2nix "almanac" inputs.almanac {
                inherit (finalHaskell) swiss-ephemeris;
              }
            );
            iCalendar = prev.haskell.lib.unmarkBroken (prev.haskell.lib.doJailbreak prevHaskell.iCalendar);
            astro-calendar = prevHaskell.callCabal2nix "astro-calendar" ./. {
              inherit (finalHaskell) almanac swiss-ephemeris iCalendar;
            };
          }
        );
      };

      packages = forAllSystems (system: {
        default = (pkgsForSystem system).haskellPackages.astro-calendar;
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
              ps.almanac
              ps.swiss-ephemeris
            ];
            withHoogle = true;
            nativeBuildInputs = [
              pkgs.haskellPackages.ghcid
              pkgs.haskellPackages.cabal-fmt
              pkgs.haskellPackages.hlint
              pkgs.haskellPackages.hoogle
              pkgs.haskellPackages.haskell-language-server
              pkgs.haskellPackages.cabal-install
              (pkgs.python3.withPackages (py: [
                py.pandas
                py.matplotlib
                py.networkx
                py.reportlab
              ]))
            ];
            # exactDeps = true; #
            shellHook = ''
              export SE_EPHE_PATH=${inputs.swisseph.outPath}/ephe
            '';
          };
        }
      );
    };
}
