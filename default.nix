{ compiler ? "ghc8107" }:

let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/21a3136d25e1652cb32197445e9799e6a5154588.tar.gz";
    sha256 = "145d474g6dngvaiwq2whqdvaq14ba9pc5pvvcz4x8l2bkwbyn3hg";
  };

  overlay = pkgsNew: pkgsOld: {
    haskell = pkgsOld.haskell // {
      packages = pkgsOld.haskell.packages // {
        "${compiler}" = pkgsOld.haskell.packages."${compiler}".override (old: {
          overrides =
            let
              oldOverrides = old.overrides or (_: _: { });

              manualOverrides = haskellPackagesNew: haskellPackagesOld: {
                simpleSurvey = pkgsNew.haskell.lib.dontCheck haskellPackagesOld.simpleSurvey;
              };

              sourceOverrides = pkgsNew.haskell.lib.packageSourceOverrides {
                simpleSurvey = ./.;
              };

            in
            pkgsNew.lib.fold pkgsNew.lib.composeExtensions oldOverrides ([ sourceOverrides ]);
        });
      };
    };
  };

  config.allowBroken = true;

  pkgs = import nixpkgs { inherit config; overlays = [ overlay ]; };

in
{ inherit (pkgs.haskell.packages."${compiler}") simpleSurvey; }
