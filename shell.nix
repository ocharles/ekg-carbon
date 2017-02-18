{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, ekg-core, network, network-carbon
      , stdenv, text, time, unordered-containers, vector
      }:
      mkDerivation {
        pname = "ekg-carbon";
        version = "1.0.7";
        src = ./.;
        libraryHaskellDepends = [
          base ekg-core network network-carbon text time unordered-containers
          vector
        ];
        homepage = "http://github.com/ocharles/ekg-carbon";
        description = "An EKG backend to send statistics to Carbon (part of Graphite monitoring tools)";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
