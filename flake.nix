{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    gitignore = {
      url = "github:hercules-ci/gitignore.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, gitignore, rust-overlay }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [ rust-overlay.overlays.default ];
      };
      rustToolchain = pkgs.rust-bin.fromRustupToolchainFile ./rust-toolchain.toml;
      rustPlatform = pkgs.makeRustPlatform {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };
    in
    {
      formatter.${system} = pkgs.nixpkgs-fmt;

      devShells.${system}.default = pkgs.mkShell {
        packages = [
          rustToolchain
          pkgs.imagemagick
          pkgs.openssl
          pkgs.pkg-config
        ];
      };

      packages.${system}.default = rustPlatform.buildRustPackage rec {
        pname = "bookdb";
        version = "0.0.0";

        src = gitignore.lib.gitignoreSource ./.;

        cargoLock = {
          lockFile = ./Cargo.lock;
        };

        buildInputs = [ pkgs.openssl.dev ];
        nativeBuildInputs = [ pkgs.pkg-config ];
        doCheck = false;

        meta = {
          description = "A database and web app to keep track of all my books.";
          homepage = "https://github.com/barrucadu/bookdb";
        };
      };
    };
}
