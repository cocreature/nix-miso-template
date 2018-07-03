{ rev ? "fdfe5b028bd4da08f0c8aabf9fb5e143ce96c56f"
, outputSha256 ? "0x0p418csdmpdfp6v4gl5ahzqhg115bb3cvrz1rb1jc7n4vxhcc8"
}:
let
  nixpkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = outputSha256;
  };
  pkgs = import nixpkgs {};

  nix-miso-template-src = pkgs.lib.cleanSourceWith {
    filter = (path: type:
      let base = baseNameOf (toString path);
      in !(pkgs.lib.hasPrefix ".ghc.environment." base)
    );
    src = pkgs.lib.cleanSource ./.;
  };

  overrides = self: super: {
    nix-miso-template = super.callCabal2nix "nix-miso-template" nix-miso-template-src {};
    http-types = super.callHackage "http-types" "0.11" {};
    miso = super.callHackage "miso" "0.20.1.0" {};
    resourcet = super.callHackage "resourcet" "1.1.11" {};
    servant = super.callHackage "servant" "0.12.1" {};
    servant-server = super.callHackage "servant-server" "0.12" {};
  };

  ghcPackages = pkgs.haskell.packages.ghc822.override(old: {
    all-cabal-hashes = builtins.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/be1df75f15b7b1b924b9b8ed506cf26fd8c48f88.tar.gz";
      sha256 = "03b4d6q2g2xkrcvn3768b3qx2fj82gpgwar77rbn6nw3850kxjqh";
    };
    overrides = self: super: overrides self super // {
      cabal-plan = pkgs.haskell.lib.overrideCabal (
        super.callCabal2nix "cabal-plan" (pkgs.fetchFromGitHub {
          owner = "hvr";
          repo = "cabal-plan";
          rev = "67d6b9b3f15fde3f3fc38d4bccc589d2e1a5420c";
          sha256 = "1rl4xaln0akcx8n7vai6ajyp16v5bg7x23f1g0ly7cvi5mvi945w";
          }) {})
        { editedCabalFile = null; };
    };
    });

  ghcjsPackages = pkgs.haskell.packages.ghcjs80.override(old: {
    all-cabal-hashes = builtins.fetchurl {
      url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/be1df75f15b7b1b924b9b8ed506cf26fd8c48f88.tar.gz";
      sha256 = "03b4d6q2g2xkrcvn3768b3qx2fj82gpgwar77rbn6nw3850kxjqh";
    };
    overrides = overrides;
  });
in
{ server = ghcPackages.nix-miso-template;
  server-shell = ghcPackages.shellFor {
    packages = p: [p.nix-miso-template];
  };
  client = ghcjsPackages.nix-miso-template;
  client-shell = ghcjsPackages.shellFor {
    packages = p: [p.nix-miso-template];
    buildInputs = [ghcPackages.cabal-plan];
  };
}
