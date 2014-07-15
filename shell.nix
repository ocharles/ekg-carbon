let
  pkgs = import <nixpkgs> {};
  haskellPackages = pkgs.haskellPackages.override {
    extension = self: super: {
      ekgCarbon = self.callPackage ./. {};
      networkCarbon = self.callPackage ../network-carbon {};
    };
  };

in pkgs.lib.overrideDerivation haskellPackages.ekgCarbon (attrs: {
     buildInputs = [ haskellPackages.cabalInstall_1_18_0_3 ] ++ attrs.buildInputs;
   })