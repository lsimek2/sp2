{
  pkgs ? import <nixpkgs> { },
}:

let
  # Define development dependencies
  devDependencies = [
    pkgs.rPackages.devtools # Package development tools
    pkgs.rPackages.testthat # Unit testing framework
    pkgs.rPackages.usethis # Package development utilities
  ];

  # Define additional R packages you want
  extraPackages = [
    pkgs.rPackages.dplyr # Data manipulation
    pkgs.rPackages.ggplot2 # Data visualization
    pkgs.rPackages.tidyverse # Collection of modern R packages
    pkgs.rPackages.MFDFA
  ];

  # Create R wrapper with all packages
  rWithPackages = pkgs.rWrapper.override {
    packages = devDependencies ++ extraPackages;
  };

  # Create RStudio wrapper with same package set
  rstudioWithPackages = pkgs.rstudioWrapper.override {
    packages = devDependencies ++ extraPackages;
  };

in
pkgs.mkShell {
  buildInputs = [
    rWithPackages
    rstudioWithPackages
    pkgs.git # Version control
  ];
}
