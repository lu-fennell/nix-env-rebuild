{ mkDerivation, applicative-quoters, attoparsec, base
, basic-prelude, containers, errors, exceptions, HUnit, lens, mtl
, optparse-applicative, pretty, pretty-show, shelly, stdenv
, system-filepath, test-framework, test-framework-hunit, text
, th-printf
}:
mkDerivation {
  pname = "nix-env-rebuild";
  version = "0.2.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    applicative-quoters attoparsec base basic-prelude containers errors
    exceptions lens mtl optparse-applicative pretty pretty-show shelly
    system-filepath text th-printf
  ];
  testHaskellDepends = [
    applicative-quoters attoparsec base basic-prelude containers errors
    exceptions HUnit lens mtl optparse-applicative pretty pretty-show
    shelly system-filepath test-framework test-framework-hunit text
    th-printf
  ];
  description = "Declarative management of the Nix user environment";
  license = "GPL";
}
