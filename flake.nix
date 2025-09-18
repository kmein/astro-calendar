{
  description = "Really simple URL shortener";

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
    inputs@{
      self,
      nixpkgs,
      swisseph,
      swiss-ephemeris,
      almanac,
    }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      compiler = "ghc912";
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsForSystem =
        system:
        import nixpkgs {
          inherit system;
          overlays = [
            self.overlays.default
          ];
        };
    in
    {
      overlays.default = final: prev: {
        astro-calendar =
          let
            haskellPackages = prev.haskell.packages.${compiler};
            swiss-ephemeris = prev.haskell.lib.doJailbreak (
              haskellPackages.callCabal2nix "swiss-ephemeris" inputs.swiss-ephemeris { }
            );
            almanac = prev.haskell.lib.doJailbreak (
              haskellPackages.callCabal2nix "almanac" inputs.almanac {
                swiss-ephemeris = swiss-ephemeris;
              }
            );
            iCalendar = prev.haskell.lib.unmarkBroken (prev.haskell.lib.doJailbreak haskellPackages.iCalendar);
          in
          haskellPackages.callCabal2nix "astro-calendar" ./. {
            inherit almanac swiss-ephemeris iCalendar;
          };
      };

      packages = forAllSystems (system: {
        default = (pkgsForSystem system).astro-calendar;
      });

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsForSystem system;
          haskellPackages = pkgs.haskell.packages.${compiler};
        in
        {
          default = haskellPackages.shellFor {
            packages = ps: [ pkgs.astro-calendar ];
            withHoogle = true;
            nativeBuildInputs = [
              pkgs.ghcid
              haskellPackages.hlint
              haskellPackages.hoogle
              haskellPackages.haskell-language-server
              haskellPackages.cabal-install
              (pkgs.python3.withPackages (py: [
                py.pandas
                py.matplotlib
                py.networkx
                py.reportlab
              ]))
            ];
            # exactDeps = true; #
            shellHook = ''
              export SE_EPHE_PATH=${swisseph.outPath}/ephe
            '';
          };
        }
      );
    };
}
