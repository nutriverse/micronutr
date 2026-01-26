# Detecting inflammation

Inflammation, and its different stages, can be assessed based on the the
levels of acute-phase proteins - one of either *c-reactive protein
(CRP)* or *α1-acid-glycoprotein (AGP)*, or both.

### Classifying inflammation based on CRP or AGP only

The classification of inflammation status based on either CRP or AGP
only is shown in the table below.

| **Acute-phase Protein** | **Cut-off Points** |
|:------------------------|:-------------------|
| CRP                     | \> 5 microgram/L   |
| AGP                     | \> 1 g/L           |

The function
[`detect_inflammation_crp()`](https://nutriverse.io/micronutr/reference/detect_inflammation.md)
classifies *c-reactive protein* levels based on the cut-off point shown
above to detect inflammation. For example, if CRP is at 2 microgram/L,

``` r
detect_inflammation_crp(crp = 2)
#> [1] "no inflammation"
```

the individual is classified as not having inflammation.

We can also set the function to provide simple integer codes to classify
inflammation by setting the `label` argument to FALSE.

``` r
detect_inflammation_crp(crp = 2, label = FALSE)
#> [1] 0
```

In this case, an integer code of 0 is provided to indicate a no
inflammation status. This would be useful in workflows that
require/prefer integer codes for binary classification.

The function
[`detect_inflammation_agp()`](https://nutriverse.io/micronutr/reference/detect_inflammation.md)
classifies *α1-acid-glycoprotein (AGP)* levels based on the cut-off
point shown above to detect inflammation. For example, if AGP is at 1.5
g/L,

``` r
detect_inflammation_agp(agp = 1.5)
#> [1] "inflammation"
```

the individual is classified as having inflammation.

We can also set the function to provide simple integer codes to classify
inflammation by setting the `label` argument to FALSE.

``` r
detect_inflammation_agp(agp = 1.5, label = FALSE)
#> [1] 1
```

In this case, an integer code of 1 is provided to indicate inflammation
status. This would be useful in workflows that require/prefer integer
codes for binary classification.

These functions are useful for classifying inflammation when data for
only one of active-phase proteins is available.

### Classifying inflammation based on both CRP and AGP

The detailed classification of inflammation status based on the
combination of CRP and AGP is shown in the table below.

| **Inflammation Status** | **Cut-off Points**                     |
|:------------------------|:---------------------------------------|
| Incubation              | CRP \> 5 microgram/L and AGP \<= 1 g/L |
| Early convalescence     | CRP \> 5 microgram/L and AGP \> 1 g/L  |
| Late convalescence      | CRP \<= 5 microgram/L and AGP \> 1 g/L |

The function
[`detect_inflammation()`](https://nutriverse.io/micronutr/reference/detect_inflammation.md)
accepts values for both CRP and AGP to classify inflammation status. For
example, an individual with CRP of 2 microgram/L and AGP of 1.5 g/L,

``` r
detect_inflammation(crp = 2, agp = 1.5)
#> [1] "late convalescence"
```

the individual is classified as being in late convalescence.

We can also set the function to provide simple integer codes to classify
inflammation by setting the `label` argument to FALSE.

``` r
detect_inflammation(crp = 2, agp = 1.5, label = FALSE)
#> [1] 3
```

In this case, an integer code of 3 is provided to indicate late
convalescence status. This would be useful in workflows that
require/prefer integer codes for classification.

The
[`detect_inflammation()`](https://nutriverse.io/micronutr/reference/detect_inflammation.md)
function can also be used for assessing inflammation status using only
one of the active-phase proteins. Using the earlier example of an
individual with a CRP of 2 µg/L,

``` r
detect_inflammation(crp = 2)
#> [1] "no inflammation"
```

we get the same result as using the
[`detect_inflammation_crp()`](https://nutriverse.io/micronutr/reference/detect_inflammation.md).

## References

C-reactive protein concentrations as a marker of inflammation or
infection for interpreting biomarkers of micronutrient status. Vitamin
and Mineral Nutrition Information System. Geneva: World Health
Organization; 2014.

  
  
