{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ pkg-config ghc cabal-install SDL2 SDL2_gfx SDL2_ttf ];
}
