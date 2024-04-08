
<!-- README.md is generated from README.Rmd. Please edit that file -->

# micronutr: Determining Vitamin and Mineral Status of Populations <img src="man/figures/logo.png" width="200px" align="right" />

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

This package provides tools for determining select vitamin and mineral
deficiencies based on World Health Organization (WHO) guidelines found
[here](https://www.who.int/teams/nutrition-and-food-safety/databases/vitamin-and-mineral-nutrition-information-system).

## What does `micronutr` do?

The `micronutr` package provides tools for determining select vitamin
and mineral deficiencies using R. Currently, `micronutr` has functions
for:

- Detecting **haemoglobinaemia** or anaemia based on an individual’s
  *serum haemoglobin* level;

- Detecting **inflammation** status based on *c-reactive protein (CRP)*
  and *alpha(1)-acid-glycoprotein (AGP)*;

- Detecting **iron deficiency** status based on an individual’s *serum
  ferritin* level;

- Detecting **iodine deficiency** status based on a population’s mean
  urinary iodine concentration.

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

## Usage

`micronutr` comes packaged with vignettes that show how to use the
package for the purposes described above.

- [Detecting
  **haemoglobinaemia**](https://nutriverse.io/micronutr/articles/haemoglobinaemia.html)

- [Detecting
  **inflammation**](https://nutriverse.io/micronutr/articles/inflammation.html)

- [Detecting **iron
  deficiency**](https://nutriverse.io/micronutr/articles/iron-deficiency.html)

- [Detecting **iodine
  deficiency**](https://nutriverse.io/micronutr/articles/iodine-deficiency.html)

## Citation

If you find the `micronutr` package useful, please cite using the
suggested citation provided by a call to the `citation` function as
follows:

``` r
citation("micronutr")
#> To cite micronutr in publications use:
#> 
#>   Ernest Guevarra, Nicholus Tint Zaw (2024). _micronutr: Determining
#>   Vitamin and Mineral Status of Populations_.
#>   doi:10.5281/zenodo.7503846 <https://doi.org/10.5281/zenodo.7503846>,
#>   R package version 0.1.1, <https://nutriverse.io/micronutr/>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {micronutr: Determining Vitamin and Mineral Status of Populations},
#>     author = {{Ernest Guevarra} and {Nicholus Tint Zaw}},
#>     year = {2024},
#>     note = {R package version 0.1.1},
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

## References

1.  Guideline on haemoglobin cutoffs to define anaemia in individuals
    and populations. Geneva: World Health Organization; 2024. Licence:
    CC BY-NC-SA 3.0 IGO.

2.  Serum ferritin concentrations for the assessment of iron status in
    individuals and populations: technical brief. Geneva: World Health
    Organization; 2020. License: CC BY-NC-SA 3.0 IGO.

3.  C-reactive protein concentrations as a marker of inflammation or
    infection for interpreting biomarkers of micronutrient status.
    Vitamin and Mineral Nutrition Information System. Geneva: World
    Health Organization; 2014.

4.  Urinary iodine concentrations for determining iodine status
    deficiency in populations. Vitamin and Mineral Nutrition Information
    System. Geneva: World Health Organization; 2013.
