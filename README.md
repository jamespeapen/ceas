# ceas <a href="https://jamespeapen.github.io/ceas/"><img src="man/figures/logo.png" align="right" height="138" style="float:right; height:138px;"/></a>

**Cellular Energetics Analysis Software**

<!-- badges: start -->
[![CRAN](https://www.r-pkg.org/badges/version/ceas)](https://cran.r-project.org/web//packages//ceas/index.html)
[![R-CMD-check](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.10067116.svg)](https://doi.org/10.5281/zenodo.10067116)

<!-- badges: end -->

## Description

Analysis and visualization of cellular energetics data from Agilent Seahorse
XF96. Cellular energetics is how cells make, use, and distribute units of energy
(primarily ATP). Measuring real-time cellular energetics is essential to
understanding a tissue or cell’s bioenergetic state and fuel dependencies. The
Seahorse machine measures a cell’s or matrix’s oxygen consumption rate (OCR) – a
proxy of oxidative phosphorylation – and extracellular acidification rate – a
proxy of glycolysis. This package offers flexible and fast analysis and plotting
capabilities for such data using the methods described by
[Mookerjee et al. (2017)](https://doi.org/10.1074/jbc.m116.774471).

#### Pronunciation

'ceas' is pronounced like the word 'seas' (siːz, SEEZ).

## Installation

### CRAN

```r
install.packages("ceas")
```

### Github

You can install the development version from Github by cloning the repo and running

```bash
git clone https://github.com/jamespeapen/ceas/
R CMD INSTALL ceas
```

You can also use the R [`devtools`](https://devtools.r-lib.org/) package:

```r
devtools::install_github("jamespeapen/ceas")
```

or [`pak`](https://pak.r-lib.org/):

```r
pak::pkg_install("jamespeapen/ceas")
```

## Usage

A user guide is available on the [package website](https://jamespeapen.github.io/ceas/).
Bug reports may be submitted through [GitHub issues](https://github.com/jamespeapen/ceas/issues).

## Contributing

Submit patches using GitHub pull requests or by sending a patch file to
<james.eapen@vai.org>. We follow the [tidyverse style guide](https://style.tidyverse.org/)
using [styler](https://styler.r-lib.org/) and [lintr](https://github.com/r-lib/lintr).

