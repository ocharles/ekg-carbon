{ cabal, ekgCore, network, networkCarbon, text, vector }:
cabal.mkDerivation (self: {
  pname = "ekg-carbon";
  version = "1.0.0";
  src = ./.;
  buildDepends = [ ekgCore network networkCarbon text vector ];
})