{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [
         pkgs.buildPackages.python39
         pkgs.buildPackages.ghc
         pkgs.buildPackages.haskellPackages.cabal-install
         pkgs.buildPackages.haskellPackages.optparse-applicative
    ];
}
