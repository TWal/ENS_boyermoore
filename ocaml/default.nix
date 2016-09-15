{ nixpkgs ? import <nixpkgs> {} }:
let
  inherit (nixpkgs) pkgs;
  packages = with pkgs.ocamlPackages; [ ];
in pkgs.stdenv.mkDerivation {
  name = "ocaml-test";
  buildInputs = with pkgs; [ ocaml gnumake ] ++ packages;
}

