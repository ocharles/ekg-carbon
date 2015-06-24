let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellngPackages.override {
    overrides = self: super: {
      ekg-carbon = self.callPackage ./. {};
      network-carbon = self.callPackage ../network-carbon {};
    };
  };
in haskellPackages.ekg-carbon.env
