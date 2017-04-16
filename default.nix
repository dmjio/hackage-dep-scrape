{ pkgs ? import <nixpkgs> {} }:
let
  ghc = pkgs.haskellPackages.ghcWithPackages (pkgs: with pkgs; [ wreq lens http-client taggy taggy-lens ]);
in pkgs.stdenv.mkDerivation {
  name = "hackage-dep-scrape";
  src = ./Main.hs;
  buildInputs = [ ghc ];
  buildCommand = ''
    ghc -threaded -rtsopts $src -o main
  '';
}
