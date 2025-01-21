{
  inputs = {
    "nixos-24.11".url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = inputs@{ self, ... }:
    let packageName = "systemd-socket-activation";
    in inputs.flake-utils.lib.eachDefaultSystem (system:
      let
        nixpkgs = {
          "nixos-24.11" = import inputs."nixos-24.11" { inherit system; };
        };
        pkgs = nixpkgs."nixos-24.11";
        project = pkgs.haskellPackages.developPackage {
          root = ./systemd-socket-activation;
          name = packageName;
        };
        inherit (pkgs.lib) fold composeExtensions concatMap attrValues;

        combineOverrides = old:
          fold composeExtensions (old.overrides or (_: _: { }));

      in {
        defaultPackage = self.packages.${system}.${packageName};

        packages = let

          inherit (pkgs.haskell.lib) dontCheck;

          makeTestConfiguration = let defaultPkgs = pkgs;
          in { pkgs ? defaultPkgs, ghcVersion, overrides ? new: old: { } }:
          let inherit (pkgs.haskell.lib) dontCheck packageSourceOverrides;
          in (pkgs.haskell.packages.${ghcVersion}.override (old: {
            overrides = combineOverrides old [
              (packageSourceOverrides {
                systemd-socket-activation = ./systemd-socket-activation;
              })
              overrides
            ];

          })).systemd-socket-activation;

        in rec {
          "${packageName}" = project;

          ghc-9-6 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc96";
          };
          ghc-9-8 = makeTestConfiguration {
            pkgs = nixpkgs."nixos-24.11";
            ghcVersion = "ghc98";
          };
          all = pkgs.symlinkJoin {
            name = packageName;
            paths = [ ghc-9-6 ghc-9-8 ];
          };
        };
      });
}
