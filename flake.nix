{
  description = "CatsForProgs";

  nixConfig.bash-prompt = "CatsForProgs $ ";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let  
        pkgs = import nixpkgs { inherit system; overlays = [ ]; };
        deps = pkgs: (with pkgs; [
          ghc
        ]);
      in {
        devShells = {
          dev = pkgs.mkShell {
            buildInputs = deps(pkgs);
          };
        };
        devShell = self.devShells."${system}".dev;
      }
    );
}
