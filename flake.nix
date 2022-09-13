{
  description = "kriti-lang";

  inputs = {
    nixpkgs.url = github:nixos/nixpkgs/nixos-22.05;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = {
    self,
    nixpkgs,
    flake-utils,
  }: let
    utils = flake-utils.lib;
  in
    utils.eachDefaultSystem (system: let
      compilerVersion = "ghc8107";
      pkgs = nixpkgs.legacyPackages.${system};
      hsPkgs = pkgs.haskell.packages.${compilerVersion}.override {
        overrides = hfinal: hprev: {
          kriti-lang = hfinal.callCabal2nix "kriti-lang" ./. {};
        };
      };
    in rec {
      packages =
        utils.flattenTree
        {kriti-lang = hsPkgs.kriti-lang;};

      devShell = hsPkgs.shellFor {
        withHoogle = true;
        packages = p: [
          p.kriti-lang
        ];
        buildInputs = [
            pkgs.cabal2nix
            pkgs.cabal-install
            pkgs.hlint
            pkgs.ormolu
            pkgs.haskell.compiler."${compilerVersion}"
            hsPkgs.ghcid
            hsPkgs.haskell-language-server
            hsPkgs.alex
            hsPkgs.happy
            hsPkgs.hspec-discover
          ]
          ++ (builtins.attrValues (import ./scripts.nix {s = pkgs.writeShellScriptBin;}));
      };

      defaultPackage = packages.kriti-lang;
    });
}
