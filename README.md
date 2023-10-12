# ceas <a href="https://jamespeapen.github.io/ceas/"><img src="man/figures/logo.png" align="right" height="138" style="float:right; height:138px;"/></a>

**Cellular Energetics Analysis Software**

<!-- badges: start -->
[![R-CMD-check](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jamespeapen/ceas/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

## Description

Analysis and visualization of cellular energetics data from Agilent Seahorse
XF96. Cellular energetics is how cells make, use, and distribute units of energy
(primarily ATP). Measuring real-time cellular energetics is essential to
understanding a tissue or cell’s bioenergetic state and fuel dependencies. The
Seahorse machine measures a cell’s or matrix’s oxygen consumption rate (OCR) – a
proxy of oxidative phosphorylation – and extracellular acidification rate – a
proxy of glycolysis. This package offers flexible and fast analysis and plotting
capabilities for such data.

## Installation

### Github

You can install from Github by cloning the repo and running

```bash
git clone https://github.com/jamespeapen/ceas/
R CMD INSTALL ceas
```

You can also use the R [`devtools`](https://devtools.r-lib.org/) package:

```r
devtools::install_github("jamespeapen/ceas")
```
