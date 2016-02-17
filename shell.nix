{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, applicative-quoters, attoparsec, base
      , basic-prelude, containers, errors, exceptions, formatting, HUnit
      , lens, mtl, optparse-applicative, pretty, pretty-show, shelly
      , stdenv, system-filepath, test-framework, test-framework-hunit
      , text, th-printf
      }:
      mkDerivation {
        pname = "nix-env-rebuild";
        version = "0.2.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          applicative-quoters attoparsec base basic-prelude containers errors
          exceptions formatting HUnit lens mtl optparse-applicative pretty
          pretty-show shelly system-filepath text th-printf
        ];
        testHaskellDepends = [
          applicative-quoters attoparsec base basic-prelude containers errors
          formatting HUnit lens mtl optparse-applicative pretty pretty-show
          shelly system-filepath test-framework test-framework-hunit text
        ];
        description = "Declarative management of nix user environments";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
