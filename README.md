# ceas <a href="https://jamespeapen.github.io/ceas/"><img src="man/figures/logo.png" align="right" height="138" style="float:right; height:138px;"/></a>

**Cellular Energetics Analysis Software**

[![DOI](https://img.shields.io/badge/DOI-10.1093%2Fbioinformatics%2Fbtae503-blue)](https://doi.org/10.1093/bioinformatics/btae503)

<!-- badges: start -->
[![CRAN release](https://www.r-pkg.org/badges/version-ago/ceas)](https://cran.r-project.org/package=ceas)
[![downloads](https://cranlogs.r-pkg.org/badges/grand-total/ceas)](https://cran.r-project.org/package=ceas)
[![R-CMD-check](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Description

Measuring cellular energetics is essential to understanding a matrix’s (e.g.
cell, tissue or biofluid) metabolic state. The Agilent Seahorse machine is a
common method to measure real-time cellular energetics, but existing analysis
tools are highly manual or lack functionality. The Cellular Energetics Analysis
Software (ceas) R package fills this analytical gap by providing modular and
automated Seahorse data analysis and visualization using the methods described
by [Mookerjee et al. (2017)](https://doi.org/10.1074/jbc.m116.774471).

#### Pronunciation

'ceas' is pronounced like the word 'seas' (siːz, SEEZ).

## Installation

### CRAN

```r
install.packages("ceas")
```

### Github

You can install the release or development versions from GitHub by cloning the
repo. The code on the `main` branch is in sync with the CRAN releases while the
`dev` branch has the latest updates. Documentation for the dev branch can be
found on the [dev page](https://jamespeapen.github.io/ceas/dev/) of the website
(`/dev`).


```bash
git clone https://github.com/jamespeapen/ceas/
git clone -b dev https://github.com/jamespeapen/ceas/ # dev version
R CMD INSTALL ceas
```

You can also use the R [`devtools`](https://devtools.r-lib.org/) package:

```r
devtools::install_github("jamespeapen/ceas")
devtools::install_github("jamespeapen/ceas", ref = "dev") # dev version
```

or [`pak`](https://pak.r-lib.org/):

```r
pak::pkg_install("jamespeapen/ceas")
pak::pkg_install("jamespeapen/ceas@dev") # dev version
```

## Usage

A user guide is available on the [package website](https://jamespeapen.github.io/ceas/).
Bug reports may be submitted through [GitHub issues](https://github.com/jamespeapen/ceas/issues).

## Citation

If you use *ceas* please cite

Rachel (Rae) J House, James P Eapen, Hui Shen, Carrie R Graveel, Matthew R
Steensma, ceas: An R package for Seahorse data analysis and visualization,
Bioinformatics, 2024;, btae503, https://doi.org/10.1093/bioinformatics/btae503

## Contributing

Submit patches using GitHub pull requests or by sending a patch file to
<james.eapen@vai.org>. We follow the [tidyverse style guide](https://style.tidyverse.org/)
using [styler](https://styler.r-lib.org/) and [lintr](https://github.com/r-lib/lintr).

