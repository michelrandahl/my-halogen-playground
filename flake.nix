{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    ps-tools.follows = "purs-nix/ps-tools";
    purs-nix.url = "github:purs-nix/purs-nix/ps-0.15";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    { nixpkgs, utils, ... }@inputs:
    utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ]
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ps-tools = inputs.ps-tools.legacyPackages.${system};
        purs-nix = inputs.purs-nix { inherit system; };

        ps = purs-nix.purs {
          dependencies = [
            "console"
            "effect"
            "halogen"
            "prelude"
            "random"
          ];

          dir = ./.;
        };
      in
      {
        packages.default = ps.bundle { };

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
            # setting up symlink for purescript-language-server
            DEV_BIN=$(pwd)/dev-bin
            PURESCRIPT_LANGUAGE_SERVER=$DEV_BIN/purescript-language-server
            mkdir -p $DEV_BIN
            if [ ! -h "$PURESCRIPT_LANGUAGE_SERVER" ]; then
              ln -s $(pwd)/node_modules/purescript-language-server/server.js $PURESCRIPT_LANGUAGE_SERVER
            fi
            # adding ./dev-bin to PATH such that neovim LSP will be able to find the LSP server
            export PATH=$DEV_BIN:$PATH
            # following is important to be able to navigate to the source and to get proper documentation in neovim
            export PURS_IDE_SOURCES=`purs-nix srcs`
            export PS1="[\e[1;32mPurescript\e[0m] $PS1"
          '';
        };
      });
}
