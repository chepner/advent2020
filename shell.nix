{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [
         pkgs.buildPackages.python39
         (pkgs.buildPackages.haskellPackages.ghcWithPackages (p: [p.optparse-applicative p.memoize]))
    ];
}
