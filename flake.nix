{
  description = "dangreco/aoc environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-parts.url = "github:hercules-ci/flake-parts";
    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    inputs@{ flake-parts, sops-nix, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "aarch64-darwin"
        "x86_64-darwin"
      ];
      perSystem =
        {
          pkgs,
          ...
        }:
        {
          devShells = {
            default = pkgs.mkShell {
              buildInputs = with pkgs; [
                pkg-config
              ];

              nativeBuildInputs = with pkgs; [
                nil
                nixd
                nixfmt

                sops
                go-task

                (haskell.packages.ghc96.ghcWithPackages (
                  hs: with hs; [
                    MissingH
                  ]
                ))
                haskell.packages.ghc96.haskell-language-server

                (pkgs.python3.withPackages (ps: [
                  (ps.buildPythonPackage rec {
                    pname = "elf";
                    version = "1.1.0";

                    src = ps.fetchPypi {
                      inherit pname version;
                      sha256 = "sha256-txsry3ACdn3m1rZ846yOfDGNP0hPMeYs+AFOQ2qcI3k=";
                    };

                    pyproject = true;

                    build-system = [
                      ps.setuptools
                    ];

                    propagatedBuildInputs = [
                      ps.beautifulsoup4
                      ps.httpx
                      ps.pydantic
                      ps.rich
                      ps.typer
                    ];

                    dontCheckRuntimeDeps = true;
                    doCheck = false;
                  })
                ]))
              ];

              shellHook = ''
                export AOC_SESSION="$(sops -d --extract '["AOC_SESSION"]' ./.sops/aoc.json)"
                export AOC_USER_AGENT="$(sops -d --extract '["AOC_USER_AGENT"]' ./.sops/aoc.json)"
              '';
            };
          };
        };
    };
}
