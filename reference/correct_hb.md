# Correct haemoglobin based on altitude and/or smoking status

Correct haemoglobin based on altitude and/or smoking status

## Usage

``` r
correct_hb(hb = NULL, alt = NULL, smoke = NULL)
```

## Arguments

- hb:

  A numeric value or numeric vector of serum haemoglobin concentration/s
  in grams per litre (g/l)

- alt:

  Altitude, in metres, above sea level

- smoke:

  Simple integer code for smoking status classification: 0 = non-smoker;
  1 = any smoker or half packet up to less than 1 packet a day; 2 = 1 up
  to less than 2 packets a day; 3 = 2 or more packets a day.

## Value

A numeric value or numeric vector of corrected serum haemoglobin
concentration/s in grams per litre (g/l)

## Author

Ernest Guevarra

## Examples

``` r
correct_hb(hb = mnData$hb[1], alt = mnData$altitude[1], smoke = 1)
#> [1] 10.2
```
