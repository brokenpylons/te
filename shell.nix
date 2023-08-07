let
  pkgs = import <nixos> {}; #import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/4d2b37a84fad1091b9de401eb450aae66f1a741e.tar.gz") {};
  ocamlPackages = pkgs.ocamlPackages;
in
pkgs.mkShell {

  #nativeBuildInputs = with ocamlPackages; [pkgs.opam dune_3 ocp-indent ocaml-lsp];
  nativeBuildInputs = with ocamlPackages; [dune_3 findlib ocaml ocp-indent ocaml-lsp];
  buildInputs = with ocamlPackages; [ppx_deriving alcotest fmt js_of_ocaml js_of_ocaml-ppx];
}
