{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ps-tools.follows = "purs-nix/ps-tools";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    utils.url = "github:numtide/flake-utils";
    buildNodeModules.url = "github:adisbladis/buildNodeModules";
  };

  outputs =
    { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ps-tools = inputs.ps-tools.legacyPackages.${system};
        #purs-nix = inputs.purs-nix { inherit system; };

        inherit (inputs.buildNodeModules.lib.${system}) buildNodeModules;
        nodeModules = buildNodeModules
          {
            packageRoot = ./.;
            inherit (pkgs) nodejs;
          };

        purs-nix = inputs.purs-nix {
          inherit system;
          overlays =
            [ (self: super:
                { "web-workers" =
                    { src.git =
                         { repo = "https://github.com/purescript-web/purescript-web-workers.git";
                           rev = "063ea75658082b600926c11f1c8e13fe6e2c4924";
                         };
                      info = {
                        version = "2.0.0";

                        dependencies = [
                          "effect"
                          "foreign"
                          "functions"
                          "maybe"
                          "newtype"
                          "prelude"
                          "unsafe-coerce"
                          "web-events"
                        ];
                      };
                    };
                }
              )
            ];
        };

        ps = purs-nix.purs {
          dependencies = [
            "console"
            "effect"
            "halogen"
            "halogen-helix"
            "prelude"
            "random"
            "web-workers"
          ];

          dir = ./.;
        };
      in
      {
        packages = {
          my-worker = ps.bundle {
            esbuild = {
              # The iife format encapsulates the worker code in a self-executing function
              format = "iife";
              minify = true;
            };
            module = "MyWorker";
          };
          default = ps.bundle {};
        };

        devShells.default = pkgs.mkShell {
          packages = with pkgs; [
            nodejs
            (ps.command { })
            purs-nix.esbuild
            purs-nix.purescript
          ];

          shellHook = ''
            # ensuring development tools are installed
            npm install -D purescript-language-server parcel
            # setting up symlink for `purescript-language-server` such that neovim LSP might find it
            DEV_BIN=$(pwd)/dev-bin
            PURESCRIPT_LANGUAGE_SERVER=$DEV_BIN/purescript-language-server
            mkdir -p $DEV_BIN
            ln -fs $(pwd)/node_modules/purescript-language-server/server.js $PURESCRIPT_LANGUAGE_SERVER
            # adding ./dev-bin to PATH such that neovim LSP will be able to find the LSP server
            export PATH=$DEV_BIN:$PATH
            # following is important to be able to navigate to the source and to get proper documentation in neovim
            export PURS_IDE_SOURCES=`purs-nix srcs`
            export PS1="[\e[1;32mPurescript\e[0m] $PS1"
          '';
        };
      });
}
