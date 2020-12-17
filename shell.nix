{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [
         pkgs.buildPackages.python39
         pkgs.buildPackages.ghcid
         (pkgs.buildPackages.haskellPackages.ghcWithHoogle (p: [
             p.optparse-applicative
             p.memoize
             p.split
             p.arithmoi
]))
    ];
}
