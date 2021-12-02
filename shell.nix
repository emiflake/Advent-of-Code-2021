{ pkgs ? import <nixpkgs> { } }:

pkgs.mkShell {
  buildInputs = with pkgs; [
    haskell.compiler.ghc8104
    cabal-install
    haskellPackages.fourmolu

    nixfmt
    haskellPackages.record-dot-preprocessor
    haskell.packages.ghc8104.haskell-language-server
    graphviz
  ];
}
