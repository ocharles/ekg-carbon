{ mkDerivation, base, ekg-core, network, network-carbon, stdenv
, text, time, unordered-containers, vector
}:
mkDerivation {
  pname = "ekg-carbon";
  version = "1.0.2";
  src = ./.;
  buildDepends = [
    base ekg-core network network-carbon text time unordered-containers
    vector
  ];
  homepage = "http://github.com/ocharles/ekg-carbon";
  description = "An EKG backend to send statistics to Carbon (part of Graphite monitoring tools)";
  license = stdenv.lib.licenses.bsd3;
}
