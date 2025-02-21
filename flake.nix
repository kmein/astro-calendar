{
  description = "Really simple URL shortener";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    swisseph.url = "github:aloistr/swisseph";
    swisseph.flake = false;
  };

  outputs = { self, nixpkgs, swisseph }: let
    supportedSystems = [ "x86_64-linux" "aarch64-linux" ];
    forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
    pkgsForSystem = system: import nixpkgs {
      inherit system;
      overlays = [self.overlays.default];
    };
  in {
    overlays.default = final: prev: {
      astro-calendar = prev.haskellPackages.callCabal2nix "astro-calendar" ./. {
        swiss-ephemeris = prev.haskell.lib.unmarkBroken (prev.haskell.lib.doJailbreak prev.haskellPackages.swiss-ephemeris);
        iCalendar = prev.haskell.lib.unmarkBroken (prev.haskell.lib.doJailbreak prev.haskellPackages.iCalendar);
      };
    };

    packages = forAllSystems (system: {
      default = (pkgsForSystem system).astro-calendar;
    });

    devShells = forAllSystems (system: let
      pkgs = pkgsForSystem system;
    in {
      default = pkgs.astro-calendar.env.overrideAttrs (old: old // {
        buildInputs = [ pkgs.ghcid pkgs.cabal-install pkgs.cabal2nix pkgs.haskellPackages.ormolu ];
        shellHook = ''
          export SE_EPHE_PATH=${swisseph.outPath}/ephe
        '';
      });
    });
  };
}
