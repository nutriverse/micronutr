# Determine population level iodine intake status

Determining population level iodine intake status using mean urinary
iodine concentration for school age children (**general**), pregnant
women (**pregnant**), or lactating women with children under 2 years old
(**lactating**).

## Usage

``` r
detect_iodine(
  iodine = NULL,
  group = c("general", "pregnant", "lactating"),
  label = TRUE
)

detect_iodine_general(iodine = NULL, label = TRUE)

detect_iodine_pregnant(iodine = NULL, label = TRUE)

detect_iodine_lactating(iodine = NULL, label = TRUE)
```

## Arguments

- iodine:

  A numeric value or numeric vector of median urinary iodine
  concentration (microgram/L) of the population or populations of
  interest.

- group:

  The population group of interest. Either school age children
  (**general**), pregnant women (**pregnant**), or lactating women with
  children under 2 years old (**lactating**)

- label:

  Logical. Should labels be used to classify iodine intake status? If
  TRUE (default), status is classified descriptively as insufficient,
  adequate, excessive, or the like. If FALSE, simple integer codes are
  returned.

## Value

If `label` is TRUE, a character value or character vector of iodine
intake status. If `label` is FALSE, an integer value or integer vector
of iodine intake status.

## Author

Nicholus Tint Zaw and Ernest Guevarra

## Examples

``` r
detect_iodine(iodine = 10)
#> [1] "insufficient (severe)"
```
