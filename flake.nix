{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
      {
        packages = {
          default = self.packages.${system}.te;

          te = pkgs.ocamlPackages.buildDunePackage {
            pname = "te";
            version = "0.0.0";
            duneVersion = "3";
            src = ./.;

            buildInputs = with pkgs.ocamlPackages; [ppx_deriving alcotest fmt js_of_ocaml js_of_ocaml-ppx];

            #strictDeps = true;
            doCheck = true;
          };
        };

        devShells = {
          default = pkgs.mkShell {
            packages = with pkgs.ocamlPackages; [
              ocaml-lsp
              ocp-indent
            ];
            inputsFrom = [
              self.packages.${system}.fp_cert
            ];
          };
        };
      });
}
