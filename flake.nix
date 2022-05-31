{
  description = "apropos-tx";

  inputs = {
    haskell-nix.url = "github:mlabs-haskell/haskell.nix";
    nixpkgs.follows = "haskell-nix/nixpkgs-unstable";
    haskell-nix.inputs.nixpkgs.follows = "haskell-nix/nixpkgs-2105";
    flake-compat-ci.url = "github:hercules-ci/flake-compat-ci";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };

    plutarch = {
      # This rev was explicitly used before
      url = "github:Plutonomicon/plutarch?rev=ae2059f11f24d47bedeaa18749d01711cddab0bc";
    };
    apropos = {
      url = "github:mlabs-haskell/apropos?ref=t4/ghc9";
    };

    # empirically discovered required overrides
    base16-bytestring = {
      # v1.0.2.0
      url = "github:haskell/base16-bytestring?rev=0965df6e4e2ed5f5d154fb5b8f83c1a9d9160f9c";
      flake = false;
    };
    cabal-doctest = {
      url = "github:haskellari/cabal-doctest?ref=v1.0.9-r1";
      flake = false;
    };
    newtype-generics = {
      url = "github:sjakobi/newtype-generics?ref=v0.6.2";
      flake = false;
    };
    cborg = {
      # v0.2.7.0
      url = "github:well-typed/cborg?rev=9df64ff5ecaebe413bdb4f5f571fa66366988722";
      flake = false;
    };
    attoparsec = {
      url = "github:haskell/attoparsec?ref=0.14.4";
      flake = false;
    };
    th-extras = {
      url = "github:mokus0/th-extras?ref=v0.0.0.6";
      flake = false;
    };
    generics-sop = {
      url = "github:well-typed/generics-sop?ref=generics-sop-0.5.1.2";
      flake = false;
    };
    constraints-extras = {
      url = "github:obsidiansystems/constraints-extras?ref=v0.3.2.1";
      flake = false;
    };
    dependent-sum = {
      url = "github:obsidiansystems/dependent-sum?ref=dependent-sum-template-0.1.1.1";
      flake = false;
    };
    recursion-schemes = {
      url = "github:recursion-schemes/recursion-schemes?ref=v5.2.2.2";
      flake = false;
    };
    cassava = {
      # v0.5.3.0 (pre)
      url = "github:haskell-hvr/cassava?rev=c821c8366ac4ce4ee3929e315ba37e694ad56f04";
      flake = false;
    };
    semialign = {
      # v1.2.0.1!
      url = "github:haskellari/these?rev=6897306f3d87aa8abd45cacaa3b24f5ab1f045a5";
      flake = false;
    };
    deriving-aeson = {
      # v0.2.8
      url = "github:fumieval/deriving-aeson?rev=0312ec75c22c26c07a70ce61fc70a7f8f2b0e9cc";
      flake = false;
    };
    quickcheck-instances = {
      url = "github:haskellari/qc-instances?ref=v0.3.27";
      flake = false;
    };
    charset = {
      url = "github:ekmett/charset?ref=v0.3.9";
      flake = false;
    };
    cereal = {
      # v0.5.8.2
      url = "github:GaloisInc/cereal?rev=b2ee49b1c3d50e5b226c4da0cb7811783ae71e94";
      flake = false;
    };
    vector-th-unbox = {
      url = "github:tsurucapital/vector-th-unbox?ref=v0.2.2";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, haskell-nix, flake-compat, flake-compat-ci, ... }:
    let
      extraSources =
        inputs.plutarch.extraSources ++
        inputs.apropos.extraSources ++
        [
          {
            src = inputs.plutarch;
            subdirs = [ "." ];
          }
          {
            src = inputs.apropos;
            subdirs = [ "." ];
          }
          # empirically discovered required overrides
          {
            src = inputs.base16-bytestring;
            subdirs = [ "." ];
          }
          {
            src = inputs.cabal-doctest;
            subdirs = [ "." ];
          }
          {
            src = inputs.newtype-generics;
            subdirs = [ "." ];
          }
          {
            src = inputs.cborg;
            subdirs = [ "./cborg" "serialise" ];
          }
          {
            src = inputs.attoparsec;
            subdirs = [ "." ];
          }
          {
            src = inputs.th-extras;
            subdirs = [ "." ];
          }
          {
            src = inputs.generics-sop;
            subdirs = [ "generics-sop" "sop-core" ];
          }
          {
            src = inputs.constraints-extras;
            subdirs = [ "." ];
          }
          {
            src = inputs.dependent-sum;
            subdirs = [ "dependent-sum" "dependent-sum-template" ];
          }
          {
            src = inputs.recursion-schemes;
            subdirs = [ "." ];
          }
          {
            src = inputs.cassava;
            subdirs = [ "." ];
          }
          {
            src = inputs.semialign;
            subdirs = [ "semialign" ];
          }
          {
            src = inputs.deriving-aeson;
            subdirs = [ "." ];
          }
          {
            src = inputs.quickcheck-instances;
            subdirs = [ "." ];
          }
          {
            src = inputs.charset;
            subdirs = [ "." ];
          }
          {
            src = inputs.cereal;
            subdirs = [ "." ];
          }
          {
            src = inputs.vector-th-unbox;
            subdirs = [ "." ];
          }
        ];

      supportedSystems = [ "x86_64-linux" ];

      perSystem = nixpkgs.lib.genAttrs supportedSystems;

      nixpkgsFor = system: import nixpkgs { inherit system; overlays = [ haskell-nix.overlay ]; inherit (haskell-nix) config; };

      fourmoluFor = system: (nixpkgsFor system).haskell-nix.tool "ghc921" "fourmolu" { };

      projectFor = system:
        let
          deferPluginErrors = true;
          pkgs = nixpkgsFor system;
        in
        (nixpkgsFor system).haskell-nix.cabalProject' {
          src = ./.;
          compiler-nix-name = "ghc921";
          cabalProjectFileName = "cabal.project";
          modules = [ (inputs.plutarch.haskellModule system) ];
          extraSources = extraSources;
          shell = {
            withHoogle = true;

            # FIXME: haskell-language-server
            # tools.haskell-language-server = { };

            exactDeps = true;

            # We use the ones from Nixpkgs, since they are cached reliably.
            # Eventually we will probably want to build these with haskell.nix.
            nativeBuildInputs = with pkgs; [
              cabal-install
              hlint
              # FIXME: fourmolu
              # (fourmoluFor system)
              fd
              haskellPackages.cabal-fmt
              nixpkgs-fmt
              coreutils
            ];

            additional = ps: [
              ps.plutarch
              ps.apropos
              ps.tasty-hedgehog

              # Overrides required by apropos-tx
              ps.base16-bytestring
              ps.cabal-doctest
              ps.newtype-generics
              ps.cborg
              ps.serialise
              ps.attoparsec
              ps.th-extras
              ps.generics-sop
              ps.constraints-extras
              ps.dependent-sum-template
              ps.dependent-sum
              ps.recursion-schemes
              ps.cassava
              ps.semialign
              ps.deriving-aeson
              ps.quickcheck-instances
              ps.charset
              ps.cereal
              ps.vector-th-unbox
            ];
          };
        };
      formatCheckFor = system:
        let
          pkgs = nixpkgsFor system;
        in
        pkgs.runCommand "format-check"
          {
            nativeBuildInputs = [ self.devShell.${system}.nativeBuildInputs ];
          } ''
          cd ${self}
          export LC_CTYPE=C.UTF-8
          export LC_ALL=C.UTF-8
          export LANG=C.UTF-8
          export IN_NIX_SHELL='pure'
          make format_check cabalfmt_check nixpkgsfmt_check lint
          mkdir $out
        '';
    in
    {
      inherit extraSources;

      project = perSystem projectFor;
      flake = perSystem (system: (projectFor system).flake { });

      # this could be done automatically, but would reduce readability
      packages = perSystem (system: self.flake.${system}.packages);
      checks = perSystem (system: self.flake.${system}.checks // {
        formatCheck = formatCheckFor system;
      });
      check = perSystem (system:
        (nixpkgsFor system).runCommand "combined-test"
          {
            nativeBuildInputs = builtins.attrValues self.checks.${system};
          } "touch $out"
      );
      apps = perSystem (system: self.flake.${system}.apps);
      devShell = perSystem (system: self.flake.${system}.devShell);

      herculesCI.ciSystems = [ "x86_64-linux" ];
    };
}
