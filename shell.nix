{ pkgs ? import <nixpkgs> {} }:
  let
    pkgDeps = with pkgs; [
      cabal-install
      haskell.compiler.ghc948
    ];
  in
    pkgs.mkShell {
      nativeBuildInputs = pkgDeps;
    }
