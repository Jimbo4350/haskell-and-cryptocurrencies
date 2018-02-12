{nixpkgs ? import <nixpkgs> { }, ghc ? nixpkgs.ghc}:

with nixpkgs;

haskell.lib.buildStackProject {
  name = "exercises-streaming";
  buildInputs = [pkgconfig zlib];
  inherit ghc;
}
