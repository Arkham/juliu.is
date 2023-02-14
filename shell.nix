{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell { buildInputs = [ yarn nodePackages.gatsby-cli ]; }
