{
  description = "Linux build environment for fcitx5";

  inputs.fcitx5-android.url = "file:///prebuilder/fcitx5-android";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };

  outputs = { self, fcitx5-android, ... }:
    let
      nixpkgs = fcitx5-android.inputs.nixpkgs;
      pkgs = import nixpkgs {
        system = "x86_64-linux";
        overlays = [ fcitx5-android.overlays.default ];
      };
    in with pkgs;
    let
      prebuilder = haskellPackages.callPackage ./nix { };
      prebuilderShell = (haskell.lib.addBuildTools prebuilder [
        haskell-language-server
        cabal2nix
        cabal-install
      ]).env;
    in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        name = "fcitx5-linux-dev";

        # 标准 Linux 构建依赖
        nativeBuildInputs = [
          cmake
          pkg-config
          autoconf
          automake
          libtool
        ];

        # 运行时依赖
        buildInputs = [
          fcitx5
          opencc
          check
          zstd
          boost
        ] ++ prebuilderShell.buildInputs;

        # 添加 Haskell 工具链
        inputsFrom = [ prebuilderShell ];

        # 设置纯 Linux 环境变量
        shellHook = ''
          export COMP_SPELL_DICT="${fcitx5}/lib/fcitx5/libexec/comp-spell-dict"
          echo "Linux build environment for fcitx5 ready"
        '';
      };

      packages.x86_64-linux.default =
        haskell.lib.justStaticExecutables prebuilder;
    };
}