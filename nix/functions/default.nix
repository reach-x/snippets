{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "dev-environment";

  buildInputs = with pkgs; [
    nodejs_18
    python311
    go
    rustc
    cargo
    git
    docker
    kubectl
  ];

  shellHook = ''
    echo "Development environment loaded"
    export PROJECT_ROOT=$(pwd)
  '';
}
