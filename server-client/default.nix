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
      in !(pkgs.lib.hasPrefix ".ghc.environment." base) &&
         !(pkgs.lib.hasSuffix ".nix" base)
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
    overrides = self: super: overrides self super // {
      servant-client-ghcjs = pkgs.haskell.lib.doJailbreak (super.callCabal2nix "servant-client-ghcjs" ((pkgs.fetchFromGitHub {
        owner = "haskell-servant";
        repo = "servant";
        rev = "544bb8184e1adbcc2359767c172b4922f8b2d650";
        sha256 = "0hkyim72sk0c1p7rwv0dggk3j8zlxpgkjl14skqkrm9yas88r5yn";
        }) + /servant-client-ghcjs) {});
      servant-client-core = super.callHackage "servant-client-core" "0.12" {};
    };
  });
in rec
{ server = pkgs.haskell.lib.justStaticExecutables (ghcPackages.nix-miso-template);
  server-shell = ghcPackages.shellFor {
    packages = p: [p.nix-miso-template];
  };
  client = ghcjsPackages.nix-miso-template;
  client-shell = ghcjsPackages.shellFor {
    packages = p: [p.nix-miso-template];
    buildInputs = [ghcPackages.cabal-plan];
  };
  docker-image = pkgs.dockerTools.buildImage {
    name = "nix-miso-template";
    contents = [ server ];
    extraCommands = ''
      mkdir -p "data"
      cp "${client}/bin/client.jsexe/all.js" "data/all.js"
    '';
    config = {
      Cmd = [ "/bin/server" "-d" "/data" ];
      ExposedPorts = {
        "8080/tcp" = {};
      };
    };
  };
}
