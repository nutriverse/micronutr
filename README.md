
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micronutr: Utilities for Calculating Indicators of Vitamin and Mineral Status of <img src="man/figures/logo.png" width="200px" align="right" />

<!-- badges: start -->

[![Project Status:
WIP](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![R-CMD-check](https://github.com/nutriverse/micronutr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/micronutr/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/nutriverse/micronutr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nutriverse/micronutr/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/micronutr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/micronutr?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/nutriverse/micronutr/badge)](https://www.codefactor.io/repository/github/nutriverse/micronutr)
<!-- badges: end -->

Vitamin and mineral deficiencies continue to be a significant public
health problem. This is particularly critical in developing countries
where deficiencies to vitamin A, iron, iodine, and other micronutrients
lead to adverse health consequences. Cross-sectional surveys are helpful
in answering questions related to the magnitude and distribution of
deficiencies of selected vitamins and minerals.

## Installation

You can install the development version of `micronutr` from
[GitHub](https://github.com/nutriverse/micronutr) with:

``` r
if(!require(remotes)) install.packages("remotes")
remotes::install_github("nutriverse/micronutr")
```

## What does `micronutr` do?

The `micronutr` package provides tools for determining select vitamin
and mineral deficiencies using R. Currently, `micronutr` has functions
for:

- detecting **haemoglobinaemia** or anaemia based on an individual’s
  *serum haemoglobin* level;

- detecting **inflammation** status based on *c-reactive protein (CRP)*
  and *alpha(1)-acid-glycoprotein (AGP)*;

- detecting **iron deficiency** status based on an individual’s *serum
  ferritin* level;

- detecting **iodine deficiency** status based on a population’s mean
  urinary iodine concentration.

<!---
These functions were developed based on current best practice described in the following guidelines, publications, and peer-reviewed journal articles:

1. 
--->

## Citation

If you find the `micronutr` package useful, please cite using the
suggested citation provided by a call to the `citation` function as
follows:

``` r
citation("micronutr")
#> Warning in citation("micronutr"): no date field in DESCRIPTION file of package
#> 'micronutr'
#> Warning in citation("micronutr"): could not determine year for 'micronutr' from
#> package DESCRIPTION file
#> 
#> To cite package 'micronutr' in publications use:
#> 
#>   Guevarra E, Tint Zaw N (????). _micronutr: Utilities for Calculating
#>   Indicators of Vitamin and Mineral Status of Populations_.
#>   https://nutriverse.io/micronutr/,
#>   https://github.com/nutriverse/micronutr.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {micronutr: Utilities for Calculating Indicators of Vitamin and Mineral Status of
#> Populations},
#>     author = {Ernest Guevarra and Nicholus {Tint Zaw}},
#>     note = {https://nutriverse.io/micronutr/, https://github.com/nutriverse/micronutr},
#>   }
```

## Community guidelines

Feedback, bug reports, and feature requests are welcome; file issues or
seek support [here](https://github.com/nutriverse/micronutr/issues). If
you would like to contribute to the package, please see our
[contributing
guidelines](https://nutriverse.io/micronutr/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
