# Determine smoking status correction factor for haemoglobin

Determine smoking status correction factor for haemoglobin

## Usage

``` r
get_smoking_correction(smoke = NULL)
```

## Arguments

- smoke:

  Simple integer code for smoking status classification: 0 = non-smoker;
  1 = any smoking or half packet up to less than 1 packet a day; 2 = 1
  up to less than 2 packets a day; 3 = 2 or more packets a day.

## Value

A numeric value or numeric vector of correction factor/s for haemoglobin
based on smoking status

## Author

Ernest Guevarra

## Examples

``` r
get_smoking_correction(smoke = 1)
#> [1] -0.3
```
