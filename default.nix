{ haskellPackages
, haskell
, zlib
}: haskellPackages.developPackage {
  root = ./.;
  modifier = drv:
    haskell.lib.addBuildTools drv (with haskellPackages;
    [
      zlib
      cabal-install
      ghcid
      hpack
      haskell-language-server
      scotty_0_22
      # wai-extra
      # text
    ]);
  source-overrides = {
    scotty = haskellPackages.scotty_0_22;
  };
}
