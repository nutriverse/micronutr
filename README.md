
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micronutr: Utilities for Calculating Indicators of Vitamin and Mineral Status of Populations <img src="man/figures/logo.png" width="200px" align="right" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/micronutr)](https://CRAN.R-project.org/package=micronutr)
[![R-CMD-check](https://github.com/nutriverse/micronutr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nutriverse/micronutr/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/nutriverse/micronutr/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/nutriverse/micronutr/actions/workflows/test-coverage.yaml)
[![Codecov test
coverage](https://codecov.io/gh/nutriverse/micronutr/branch/main/graph/badge.svg)](https://app.codecov.io/gh/nutriverse/micronutr?branch=main)
[![CodeFactor](https://www.codefactor.io/repository/github/nutriverse/micronutr/badge)](https://www.codefactor.io/repository/github/nutriverse/micronutr)
[![DOI](https://zenodo.org/badge/584725138.svg)](https://zenodo.org/badge/latestdoi/584725138)
<!-- badges: end -->

Vitamin and mineral deficiencies continue to be a significant public
health problem. This is particularly critical in developing countries
where deficiencies to vitamin A, iron, iodine, and other micronutrients
lead to adverse health consequences. Cross-sectional surveys are helpful
in answering questions related to the magnitude and distribution of
deficiencies of selected vitamins and minerals.

## Installation

You can install `micronutr` from [CRAN](https://cran.r-project.org)
with:

``` r
install.packages("micronutr")
```

You can install the development version of `micronutr` from [nutriverse
r-universe](https://nutriverse.r-universe.dev) with:

``` r
install.packages(
  "micronutr", 
  repos = c('https://nutriverse.r-universe.dev', 'https://cloud.r-project.org')
)
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

## Usage

`micronutr` comes packaged with vignettes that show how to use the
package for the purposes described above.

## Citation

If you find the `micronutr` package useful, please cite using the
suggested citation provided by a call to the `citation` function as
follows:

``` r
citation("micronutr")
#> To cite micronutr in publications use:
#> 
#>   Ernest Guevarra, Nicholus Tint Zaw (2024). _micronutr: Utilities for
#>   Calculating Indicators of Vitamin and Mineral Status of Populations_.
#>   doi:10.5281/zenodo.7503846 <https://doi.org/10.5281/zenodo.7503846>,
#>   R package version 0.1.0, <https://nutriverse.io/micronutr/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {micronutr: Utilities for Calculating Indicators of Vitamin and Mineral Status of Populations},
#>     author = {{Ernest Guevarra} and {Nicholus Tint Zaw}},
#>     year = {2024},
#>     note = {R package version 0.1.0},
#>     url = {https://nutriverse.io/micronutr/},
#>     doi = {10.5281/zenodo.7503846},
#>   }
```

## Community guidelines

Feedback, bug reports, and feature requests are welcome; file issues or
seek support [here](https://github.com/nutriverse/micronutr/issues). If
you would like to contribute to the package, please see our
[contributing
guidelines](https://nutriverse.io/micronutr/CONTRIBUTING.html).

This project is released with a [Contributor Code of
Conduct](https://nutriverse.io/micronutr/CODE_OF_CONDUCT.html). By
contributing to this project, you agree to abide by its terms.
