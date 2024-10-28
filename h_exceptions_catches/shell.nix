{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.haskell.compiler.native-bignum.ghc948  # Replace with your desired GHC version
    pkgs.haskellPackages.cabal-install
  ];
}
