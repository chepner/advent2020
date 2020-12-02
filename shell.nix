{ pkgs ? import <nixpkgs> {} }:
  pkgs.mkShell {
    nativeBuildInputs = [
         pkgs.buildPackages.python39
         pkgs.buildPackages.ghc
    ];
}
