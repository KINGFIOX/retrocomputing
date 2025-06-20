{ sources ? import ./sources.nix }:

let
  overlay = _: pkgs: {

    # Nix tooling
    niv = (import sources.niv {}).niv;
    gitignore = import sources.gitignore { inherit (pkgs) lib; };

    # Haskell overrides
    haskellPackages = pkgs.haskellPackages.override {
      overrides = self: super: {
        # Add overrides here
        clash-prelude =
          self.callCabal2nix "clash-prelude" (sources.clash-compiler + "/clash-prelude") {};
        clash-lib =
          self.callCabal2nix "clash-lib" (sources.clash-compiler + "/clash-lib") {};
        clash-ghc =
          self.callCabal2nix "clash-ghc" (sources.clash-compiler + "/clash-ghc") {};
      };
    };
  };

in import sources.nixpkgs { overlays = [ overlay ]; }
