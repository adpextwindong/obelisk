{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; with haskellPackages; [
    cabal-install
    haskell.compiler.ghc8107
    SDL2
    SDL2_gfx
    SDL2_ttf
    pkg-config
    ];
}
