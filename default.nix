with import <nixpkgs> {};

let
  rlibs = with rPackages; [
    R
    readxl
    ggplot2
    roxygen2
    data_table
  ];

  _libs = with pkgs; [
    texlive.combined.scheme-basic
  ];


in mkShell {
  buildInputs = [
      rlibs
      _libs
  ];
  shellHook = ''
    mkdir -p "$HOME/.R"
    export R_LIBS_USER="$HOME/.R"
  '';

}
