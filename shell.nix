let
  pkgs = import <nixpkgs> {}; #import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz") {};
  ocamlPackages = pkgs.ocaml-ng.ocamlPackages_4_14;
in
ocamlPackages.buildDunePackage rec {
  pname = "engine";
  version = "1.0.0";

  nativeBuildInputs = with ocamlPackages; [pkgs.opam dune_3 ocp-indent ocaml-lsp];
  #buildInputs = with ocamlPackages; [ppx_deriving alcotest uutf fmt js_of_ocaml js_of_ocaml-ppx];
}

