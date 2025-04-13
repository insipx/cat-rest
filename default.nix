{ haskell
, zlib
, haskellPackages
, ...
}@pkgs:
let
  haskellPackages = pkgs.haskellPackages.override {
    overrides = self: super: {
      scotty = self.scotty_0_22;
    };
  };
in
haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    haskell.lib.addBuildTools drv
      (with haskellPackages;[
        zlib
        cabal-install
        ghcid
        hpack
        haskell-language-server
        scotty
      ]);
}
