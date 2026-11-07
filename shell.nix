{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    nodejs_24
    (yarn.override { nodejs = nodejs_24; })
  ];
}
