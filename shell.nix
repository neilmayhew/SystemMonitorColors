{ nixpkgs ? import <nixpkgs> {} }:

with nixpkgs;

mkShell rec {
  buildInputs = [
    (haskellPackages.ghcWithPackages (pkgs: with pkgs; [
      Color
      hspec
      optparse-applicative
      terminal-size
    ]))
    stress
    dconf
  ];
}
