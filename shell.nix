{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, applicative-quoters, attoparsec, base
      , basic-prelude, containers, errors, formatting, HUnit, lens, mtl
      , optparse-applicative, pretty, pretty-show, shelly, stdenv
      , system-filepath, test-framework, test-framework-hunit, text
      }:
      mkDerivation {
        pname = "nix-tools";
        version = "0.5.0.1";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          applicative-quoters attoparsec base basic-prelude containers errors
          formatting HUnit lens mtl optparse-applicative pretty pretty-show
          shelly system-filepath test-framework test-framework-hunit text
        ];
        description = "Tools for nixos";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
