{
  description = "Flake to get iscream development environment";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: let
      pkgs = nixpkgs.legacyPackages.${system};

      Imports = with pkgs.rPackages; [
        data_table
        ggplot2
        readxl
        lme4
      ];

      rDevDeps = with pkgs.rPackages; [
        covr
        cffr
        devtools
        DT
        htmltools
        roxygen2
        pkgdown
        spelling
        testthat
        usethis
        V8
        rhub
        curl
      ];

      htslib = pkgs.htslib.overrideAttrs (finalAttrs: previousAttrs: {
        buildInputs = previousAttrs.buildInputs ++ [ pkgs.libdeflate ];
      });
      sysDeps = with pkgs; [
        R
      ];

      sysDevDeps = with pkgs; [
        air-formatter
        checkbashisms
        html-tidy
        qpdf
        (pkgs.texlive.combine {
          inherit (pkgs.texlive) scheme-medium
          inconsolata
          xkeyval
          etoolbox;
        })
      ];

      # default package
      rDeps = [ Imports ];
      ceas = pkgs.rPackages.buildRPackage {
        name = "ceas";
        src = self;
        nativeBuildInputs = sysDeps;
        propagatedBuildInputs = rDeps;
      };
      # Create R development environment with iscream and other useful libraries
      rvenv = pkgs.rWrapper.override {
        packages = rDeps ++ rDevDeps ++ sysDeps ++ sysDevDeps;
      };
    in {
      packages.default = ceas;
      devShells.default = pkgs.mkShell {
          buildInputs = rDeps ++ rDevDeps ++ sysDeps ++ sysDevDeps;
          inputsFrom = pkgs.lib.singleton ceas;
          packages = pkgs.lib.singleton rvenv;
          shellHook = ''
            mkdir -p "$HOME/.R"
            export R_LIBS_USER="$HOME/.R"
          '';
      };
    });
}
