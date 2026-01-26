# Micronutrient survey data

This sample dataset is from a micronutrient survey in a country in East
Africa with 19449 observations and 9 variables described below.

## Usage

``` r
mnData
```

## Format

A tibble with 9 columns and 19449 rows.

|               |                                                        |
|---------------|--------------------------------------------------------|
| **Variables** | **Description**                                        |
| *psu*         | Primary sampling unit identifier                       |
| *sex*         | Sex (1 = Male; 2 = Female)                             |
| *age*         | Age (in years)                                         |
| *group*       | Group classification                                   |
| *hb*          | Haemoglobin values (g/L)                               |
| *calcium*     | Serum calcium values ()                                |
| *crp*         | Serum c-reactive protein (microgram/l)                 |
| *ferritin*    | Serum ferritin ()                                      |
| *iodine*      | Urine iodine ()                                        |
| *altitude*    | Altitude of sampling location above sea level (metres) |

## Source

From a micronutrient survey in a country in East Africa

## Examples

``` r
mnData
#> # A tibble: 19,449 × 10
#>      psu   sex   age group             hb calcium   crp ferritin iodine altitude
#>    <dbl> <dbl> <dbl> <chr>          <dbl>   <dbl> <dbl>    <dbl>  <dbl>    <int>
#>  1    45     1     3 child           10.5     8.9   3.6     11.3     NA      978
#>  2    45     2     2 child           10.3     9.8   6.3     15.6     NA      978
#>  3    45     1     0 child            8.8    NA    NA       NA       NA      978
#>  4    45     2    25 lactating       13.1     9.1   2.5     20.3      0      978
#>  5    45     2    26 lactating       12.6     9.6   4.2     18.6      0      978
#>  6    45     2    26 non-pregnant …  10.4     9.9   5        6       NA      978
#>  7    45     2    30 lactating        9.1    10.3   3.1      3        0      978
#>  8    45     2    30 lactating       11.6    10.4   0.7     25.6     NA      978
#>  9    45     2     4 child           12.4    NA    NA       16.4     NA      978
#> 10    45     2    30 lactating       13.3    NA    NA       NA        0      978
#> # ℹ 19,439 more rows
```
