# Determine iron storage status

Given serum ferritin values, determine iron storage status.

## Usage

``` r
detect_iron_deficiency_u5(ferritin = NULL, label = TRUE)

detect_iron_deficiency_5over(ferritin = NULL, label = TRUE)

detect_iron_deficiency(ferritin = NULL, group = c("u5", "5over"), label = TRUE)

detect_iron_deficiency_qualitative(
  ferritin = NULL,
  inflammation = NULL,
  group = c("u5", "5over"),
  label = TRUE
)
```

## Arguments

- ferritin:

  A numeric value or numeric vector of serum ferritin level in
  micrograms per litre (microgram/L).

- label:

  Logical. Should labels be used to classify iron storage status? If
  TRUE (default), status is classified as "no iron deficiency" or "iron
  deficiency". If FALSE, simple integer codes are returned: 0 for no
  iron deficiency and 1 for iron deficiency.

- group:

  A character value specifying the population target group to determine
  iron status from. Can be either for under 5 year old ("u5") or 5 years
  and over ("5over"). Default to "u5".

- inflammation:

  Logical value or vector. Is subject in inflammation or not?

## Value

If `label` is TRUE, a character value or character vector of iron status
classification (can be either "iron deficiency" or "no iron
deficiency"). If `label` is FALSE, an integer value or integer vector of
iron status classification (0 = no iron deficiency; 1 = iron deficiency)

## Author

Nicholus Tint Zaw and Ernest Guevarra

## Examples

``` r
 # Iron storage status based on CRP only
 ferritin_corrected <- correct_ferritin(
   crp = mnData$crp, ferritin = mnData$ferritin
 )
 detect_iron_deficiency(ferritin_corrected)
#>     [1] "iron deficiency"    "iron deficiency"    NA                  
#>     [4] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>     [7] "iron deficiency"    "no iron deficiency" NA                  
#>    [10] NA                   "iron deficiency"    NA                  
#>    [13] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>    [16] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>    [19] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>    [22] NA                   "no iron deficiency" "no iron deficiency"
#>    [25] NA                   "iron deficiency"    NA                  
#>    [28] "iron deficiency"    NA                   NA                  
#>    [31] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>    [34] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>    [37] NA                   "no iron deficiency" "no iron deficiency"
#>    [40] "no iron deficiency" "no iron deficiency" NA                  
#>    [43] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>    [46] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>    [49] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>    [52] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>    [55] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>    [58] "no iron deficiency" "no iron deficiency" NA                  
#>    [61] NA                   "no iron deficiency" "iron deficiency"   
#>    [64] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>    [67] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>    [70] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>    [73] "iron deficiency"    NA                   "no iron deficiency"
#>    [76] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>    [79] "no iron deficiency" NA                   "no iron deficiency"
#>    [82] "no iron deficiency" NA                   "iron deficiency"   
#>    [85] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>    [88] "iron deficiency"    NA                   NA                  
#>    [91] NA                   NA                   NA                  
#>    [94] NA                   "iron deficiency"    "no iron deficiency"
#>    [97] NA                   NA                   "no iron deficiency"
#>   [100] NA                   NA                   "no iron deficiency"
#>   [103] "no iron deficiency" NA                   "no iron deficiency"
#>   [106] NA                   "no iron deficiency" NA                  
#>   [109] "no iron deficiency" "no iron deficiency" NA                  
#>   [112] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [115] NA                   "no iron deficiency" NA                  
#>   [118] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [121] "iron deficiency"    NA                   "no iron deficiency"
#>   [124] NA                   "no iron deficiency" "iron deficiency"   
#>   [127] "iron deficiency"    "no iron deficiency" NA                  
#>   [130] "no iron deficiency" NA                   "iron deficiency"   
#>   [133] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [136] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [139] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [142] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [145] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [148] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [151] NA                   NA                   "iron deficiency"   
#>   [154] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [157] NA                   NA                   "no iron deficiency"
#>   [160] "iron deficiency"    NA                   NA                  
#>   [163] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [166] "iron deficiency"    NA                   NA                  
#>   [169] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [172] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [175] NA                   NA                   "iron deficiency"   
#>   [178] "iron deficiency"    NA                   "iron deficiency"   
#>   [181] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [184] NA                   "iron deficiency"    "no iron deficiency"
#>   [187] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [190] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [193] "iron deficiency"    NA                   "iron deficiency"   
#>   [196] NA                   NA                   NA                  
#>   [199] "iron deficiency"    NA                   "no iron deficiency"
#>   [202] NA                   NA                   NA                  
#>   [205] NA                   NA                   NA                  
#>   [208] NA                   NA                   NA                  
#>   [211] NA                   "iron deficiency"    "no iron deficiency"
#>   [214] "iron deficiency"    NA                   NA                  
#>   [217] NA                   "no iron deficiency" NA                  
#>   [220] NA                   NA                   NA                  
#>   [223] NA                   "no iron deficiency" NA                  
#>   [226] NA                   NA                   "no iron deficiency"
#>   [229] "iron deficiency"    NA                   NA                  
#>   [232] "iron deficiency"    NA                   NA                  
#>   [235] NA                   NA                   NA                  
#>   [238] "iron deficiency"    "iron deficiency"    NA                  
#>   [241] NA                   "no iron deficiency" NA                  
#>   [244] NA                   NA                   "iron deficiency"   
#>   [247] NA                   NA                   NA                  
#>   [250] NA                   "no iron deficiency" "iron deficiency"   
#>   [253] "no iron deficiency" "iron deficiency"    NA                  
#>   [256] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [259] "no iron deficiency" NA                   "iron deficiency"   
#>   [262] NA                   NA                   NA                  
#>   [265] NA                   "no iron deficiency" "iron deficiency"   
#>   [268] "no iron deficiency" NA                   "iron deficiency"   
#>   [271] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [274] NA                   NA                   "no iron deficiency"
#>   [277] "no iron deficiency" NA                   "no iron deficiency"
#>   [280] NA                   NA                   "iron deficiency"   
#>   [283] "iron deficiency"    "no iron deficiency" NA                  
#>   [286] "iron deficiency"    NA                   "no iron deficiency"
#>   [289] NA                   "iron deficiency"    "no iron deficiency"
#>   [292] "no iron deficiency" NA                   "iron deficiency"   
#>   [295] "iron deficiency"    "no iron deficiency" NA                  
#>   [298] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [301] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [304] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [307] NA                   "no iron deficiency" "no iron deficiency"
#>   [310] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [313] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [316] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [319] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [322] "iron deficiency"    NA                   NA                  
#>   [325] "iron deficiency"    "no iron deficiency" NA                  
#>   [328] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [331] NA                   NA                   "no iron deficiency"
#>   [334] "iron deficiency"    NA                   NA                  
#>   [337] "no iron deficiency" NA                   NA                  
#>   [340] "no iron deficiency" "no iron deficiency" NA                  
#>   [343] "no iron deficiency" "no iron deficiency" NA                  
#>   [346] "iron deficiency"    NA                   "no iron deficiency"
#>   [349] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [352] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [355] NA                   NA                   "iron deficiency"   
#>   [358] "no iron deficiency" NA                   "iron deficiency"   
#>   [361] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [364] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [367] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [370] NA                   "no iron deficiency" "no iron deficiency"
#>   [373] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [376] NA                   "no iron deficiency" "no iron deficiency"
#>   [379] NA                   "iron deficiency"    "iron deficiency"   
#>   [382] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [385] "iron deficiency"    NA                   "no iron deficiency"
#>   [388] "no iron deficiency" "no iron deficiency" NA                  
#>   [391] "iron deficiency"    "no iron deficiency" NA                  
#>   [394] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [397] "no iron deficiency" "iron deficiency"    NA                  
#>   [400] NA                   "no iron deficiency" "no iron deficiency"
#>   [403] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [406] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [409] NA                   "iron deficiency"    "no iron deficiency"
#>   [412] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [415] NA                   "iron deficiency"    NA                  
#>   [418] "iron deficiency"    NA                   "no iron deficiency"
#>   [421] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [424] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [427] "no iron deficiency" NA                   "iron deficiency"   
#>   [430] "no iron deficiency" "no iron deficiency" NA                  
#>   [433] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [436] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [439] NA                   "iron deficiency"    "iron deficiency"   
#>   [442] NA                   "no iron deficiency" "no iron deficiency"
#>   [445] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [448] "no iron deficiency" NA                   "no iron deficiency"
#>   [451] "no iron deficiency" NA                   "iron deficiency"   
#>   [454] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [457] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [460] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [463] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [466] "no iron deficiency" "no iron deficiency" NA                  
#>   [469] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [472] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [475] "no iron deficiency" NA                   "no iron deficiency"
#>   [478] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [481] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [484] "iron deficiency"    "iron deficiency"    NA                  
#>   [487] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [490] "no iron deficiency" NA                   "no iron deficiency"
#>   [493] "no iron deficiency" "no iron deficiency" NA                  
#>   [496] NA                   "no iron deficiency" NA                  
#>   [499] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [502] NA                   "iron deficiency"    "no iron deficiency"
#>   [505] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [508] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [511] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [514] NA                   "no iron deficiency" "no iron deficiency"
#>   [517] NA                   "iron deficiency"    "iron deficiency"   
#>   [520] "no iron deficiency" NA                   "no iron deficiency"
#>   [523] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [526] "no iron deficiency" NA                   "no iron deficiency"
#>   [529] NA                   NA                   NA                  
#>   [532] NA                   "no iron deficiency" "no iron deficiency"
#>   [535] "iron deficiency"    NA                   "no iron deficiency"
#>   [538] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [541] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [544] NA                   "no iron deficiency" "no iron deficiency"
#>   [547] "iron deficiency"    NA                   "iron deficiency"   
#>   [550] "iron deficiency"    NA                   "iron deficiency"   
#>   [553] "no iron deficiency" NA                   "no iron deficiency"
#>   [556] "iron deficiency"    "iron deficiency"    NA                  
#>   [559] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [562] NA                   "no iron deficiency" "iron deficiency"   
#>   [565] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [568] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [571] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [574] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [577] "no iron deficiency" NA                   NA                  
#>   [580] NA                   "no iron deficiency" "no iron deficiency"
#>   [583] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [586] "iron deficiency"    NA                   NA                  
#>   [589] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [592] "no iron deficiency" NA                   "iron deficiency"   
#>   [595] "no iron deficiency" NA                   "no iron deficiency"
#>   [598] NA                   NA                   NA                  
#>   [601] NA                   NA                   NA                  
#>   [604] NA                   NA                   NA                  
#>   [607] NA                   NA                   NA                  
#>   [610] NA                   "no iron deficiency" NA                  
#>   [613] NA                   NA                   "iron deficiency"   
#>   [616] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [619] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [622] NA                   NA                   "iron deficiency"   
#>   [625] NA                   "no iron deficiency" "no iron deficiency"
#>   [628] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [631] NA                   NA                   "no iron deficiency"
#>   [634] "no iron deficiency" NA                   "no iron deficiency"
#>   [637] "no iron deficiency" NA                   NA                  
#>   [640] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [643] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [646] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [649] NA                   "no iron deficiency" "iron deficiency"   
#>   [652] "no iron deficiency" "iron deficiency"    NA                  
#>   [655] NA                   "no iron deficiency" "iron deficiency"   
#>   [658] "no iron deficiency" "no iron deficiency" NA                  
#>   [661] NA                   "no iron deficiency" "iron deficiency"   
#>   [664] NA                   "no iron deficiency" "no iron deficiency"
#>   [667] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [670] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [673] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [676] NA                   "no iron deficiency" "iron deficiency"   
#>   [679] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [682] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [685] "no iron deficiency" "no iron deficiency" NA                  
#>   [688] "iron deficiency"    "no iron deficiency" NA                  
#>   [691] NA                   NA                   "iron deficiency"   
#>   [694] "no iron deficiency" NA                   "no iron deficiency"
#>   [697] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [700] "no iron deficiency" "no iron deficiency" NA                  
#>   [703] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [706] "no iron deficiency" "no iron deficiency" NA                  
#>   [709] "no iron deficiency" NA                   NA                  
#>   [712] NA                   "no iron deficiency" "iron deficiency"   
#>   [715] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [718] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [721] "no iron deficiency" NA                   "iron deficiency"   
#>   [724] "iron deficiency"    NA                   NA                  
#>   [727] "no iron deficiency" "no iron deficiency" NA                  
#>   [730] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [733] "no iron deficiency" NA                   "no iron deficiency"
#>   [736] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [739] "no iron deficiency" "no iron deficiency" NA                  
#>   [742] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [745] "no iron deficiency" "iron deficiency"    NA                  
#>   [748] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [751] "no iron deficiency" NA                   "no iron deficiency"
#>   [754] NA                   "no iron deficiency" NA                  
#>   [757] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [760] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [763] NA                   NA                   "iron deficiency"   
#>   [766] "iron deficiency"    "no iron deficiency" NA                  
#>   [769] "iron deficiency"    "no iron deficiency" NA                  
#>   [772] NA                   NA                   "iron deficiency"   
#>   [775] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [778] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [781] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [784] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [787] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [790] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [793] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [796] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [799] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [802] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [805] "iron deficiency"    "iron deficiency"    NA                  
#>   [808] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [811] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [814] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [817] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [820] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [823] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [826] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [829] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [832] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [835] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [838] "iron deficiency"    NA                   "iron deficiency"   
#>   [841] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [844] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [847] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [850] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [853] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [856] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [859] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [862] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [865] "no iron deficiency" NA                   "no iron deficiency"
#>   [868] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [871] "no iron deficiency" NA                   "no iron deficiency"
#>   [874] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [877] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [880] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [883] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [886] NA                   "iron deficiency"    "iron deficiency"   
#>   [889] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [892] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>   [895] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [898] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [901] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [904] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [907] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [910] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [913] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>   [916] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [919] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [922] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [925] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [928] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [931] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [934] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [937] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [940] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [943] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [946] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [949] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [952] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>   [955] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [958] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [961] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [964] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>   [967] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [970] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>   [973] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [976] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [979] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [982] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [985] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>   [988] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>   [991] "no iron deficiency" "iron deficiency"    NA                  
#>   [994] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>   [997] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1000] "iron deficiency"    "iron deficiency"    NA                  
#>  [1003] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1006] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1009] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1012] "no iron deficiency" NA                   "no iron deficiency"
#>  [1015] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1018] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1021] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1024] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1027] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1030] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1033] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1036] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1039] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1042] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1045] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1048] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1051] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1054] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1057] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1060] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1063] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1066] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1069] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1072] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1075] "iron deficiency"    "no iron deficiency" NA                  
#>  [1078] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1081] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1084] "no iron deficiency" "iron deficiency"    NA                  
#>  [1087] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1090] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1093] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1096] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1099] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1102] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1105] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1108] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1111] NA                   "iron deficiency"    "no iron deficiency"
#>  [1114] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1117] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1120] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1123] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1126] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1129] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1132] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1135] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1138] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1141] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1144] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1147] "no iron deficiency" "iron deficiency"    NA                  
#>  [1150] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1153] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1156] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1159] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1162] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1165] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1168] "no iron deficiency" NA                   "no iron deficiency"
#>  [1171] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1174] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1177] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1180] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1183] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1186] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1189] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1192] NA                   "iron deficiency"    "no iron deficiency"
#>  [1195] "iron deficiency"    "no iron deficiency" NA                  
#>  [1198] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1201] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1204] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1207] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1210] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1213] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1216] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1219] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1222] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1225] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1228] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1231] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1234] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1237] "iron deficiency"    NA                   "iron deficiency"   
#>  [1240] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1243] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1246] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1249] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1252] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1255] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1258] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1261] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1264] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1267] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1270] NA                   "no iron deficiency" "iron deficiency"   
#>  [1273] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1276] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1279] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1282] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1285] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1288] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1291] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1294] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1297] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1300] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1303] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1306] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1309] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1312] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1315] "no iron deficiency" NA                   "iron deficiency"   
#>  [1318] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1321] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1324] "no iron deficiency" "no iron deficiency" NA                  
#>  [1327] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1330] NA                   "iron deficiency"    "no iron deficiency"
#>  [1333] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1336] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1339] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1342] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1345] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1348] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1351] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1354] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1357] "iron deficiency"    NA                   "iron deficiency"   
#>  [1360] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1363] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1366] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1369] NA                   NA                   "iron deficiency"   
#>  [1372] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1375] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1378] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1381] "iron deficiency"    NA                   "no iron deficiency"
#>  [1384] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1387] "no iron deficiency" NA                   "iron deficiency"   
#>  [1390] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1393] "no iron deficiency" "no iron deficiency" NA                  
#>  [1396] NA                   NA                   "no iron deficiency"
#>  [1399] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1402] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1405] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1408] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1411] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1414] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1417] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1420] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1423] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1426] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1429] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1432] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1435] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1438] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1441] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1444] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1447] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1450] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1453] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1456] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1459] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1462] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1465] "no iron deficiency" NA                   "iron deficiency"   
#>  [1468] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1471] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1474] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1477] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1480] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1483] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1486] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1489] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1492] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1495] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1498] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1501] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1504] "no iron deficiency" "iron deficiency"    NA                  
#>  [1507] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1510] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1513] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1516] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1519] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1522] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1525] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1528] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1531] NA                   "iron deficiency"    "no iron deficiency"
#>  [1534] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1537] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1540] "iron deficiency"    NA                   "no iron deficiency"
#>  [1543] "no iron deficiency" NA                   "no iron deficiency"
#>  [1546] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1549] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1552] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1555] "iron deficiency"    NA                   "no iron deficiency"
#>  [1558] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1561] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1564] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1567] NA                   "no iron deficiency" "iron deficiency"   
#>  [1570] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1573] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1576] "no iron deficiency" "iron deficiency"    NA                  
#>  [1579] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1582] "no iron deficiency" NA                   "iron deficiency"   
#>  [1585] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1588] NA                   "iron deficiency"    "no iron deficiency"
#>  [1591] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1594] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1597] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1600] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1603] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1606] "no iron deficiency" "iron deficiency"    NA                  
#>  [1609] NA                   "no iron deficiency" "iron deficiency"   
#>  [1612] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1615] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1618] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1621] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1624] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1627] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1630] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1633] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1636] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1639] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1642] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1645] "iron deficiency"    NA                   "iron deficiency"   
#>  [1648] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1651] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1654] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1657] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1660] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1663] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1666] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1669] "no iron deficiency" NA                   "iron deficiency"   
#>  [1672] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1675] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1678] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1681] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1684] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1687] "no iron deficiency" "no iron deficiency" NA                  
#>  [1690] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1693] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1696] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1699] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1702] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1705] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1708] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1711] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1714] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1717] "iron deficiency"    NA                   "iron deficiency"   
#>  [1720] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1723] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1726] NA                   "iron deficiency"    "iron deficiency"   
#>  [1729] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1732] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1735] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1738] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1741] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1744] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1747] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1750] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1753] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1756] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1759] "no iron deficiency" "no iron deficiency" NA                  
#>  [1762] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1765] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1768] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1771] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1774] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1777] "no iron deficiency" "no iron deficiency" NA                  
#>  [1780] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1783] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1786] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1789] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1792] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1795] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1798] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1801] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1804] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1807] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1810] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1813] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1816] "iron deficiency"    "iron deficiency"    NA                  
#>  [1819] "iron deficiency"    "iron deficiency"    NA                  
#>  [1822] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1825] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1828] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1831] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1834] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1837] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1840] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1843] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1846] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1849] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1852] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1855] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1858] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1861] NA                   "no iron deficiency" NA                  
#>  [1864] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [1867] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1870] NA                   NA                   "iron deficiency"   
#>  [1873] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1876] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1879] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1882] NA                   "no iron deficiency" NA                  
#>  [1885] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1888] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1891] "iron deficiency"    NA                   "no iron deficiency"
#>  [1894] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1897] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1900] "no iron deficiency" NA                   "no iron deficiency"
#>  [1903] NA                   "no iron deficiency" NA                  
#>  [1906] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1909] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1912] NA                   "no iron deficiency" "no iron deficiency"
#>  [1915] "no iron deficiency" "no iron deficiency" NA                  
#>  [1918] "no iron deficiency" "no iron deficiency" NA                  
#>  [1921] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1924] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1927] "no iron deficiency" "no iron deficiency" NA                  
#>  [1930] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [1933] NA                   NA                   "no iron deficiency"
#>  [1936] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1939] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1942] NA                   "no iron deficiency" NA                  
#>  [1945] NA                   "no iron deficiency" NA                  
#>  [1948] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [1951] "iron deficiency"    NA                   "no iron deficiency"
#>  [1954] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [1957] NA                   "no iron deficiency" "iron deficiency"   
#>  [1960] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1963] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1966] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [1969] NA                   NA                   "no iron deficiency"
#>  [1972] "iron deficiency"    "no iron deficiency" NA                  
#>  [1975] NA                   NA                   "iron deficiency"   
#>  [1978] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [1981] "no iron deficiency" "no iron deficiency" NA                  
#>  [1984] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [1987] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [1990] "no iron deficiency" NA                   "no iron deficiency"
#>  [1993] NA                   "no iron deficiency" "iron deficiency"   
#>  [1996] NA                   NA                   "iron deficiency"   
#>  [1999] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2002] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2005] "iron deficiency"    NA                   "no iron deficiency"
#>  [2008] "no iron deficiency" "iron deficiency"    NA                  
#>  [2011] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2014] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2017] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2020] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2023] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2026] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2029] "no iron deficiency" NA                   "iron deficiency"   
#>  [2032] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2035] "no iron deficiency" "no iron deficiency" NA                  
#>  [2038] NA                   "no iron deficiency" "no iron deficiency"
#>  [2041] "no iron deficiency" "no iron deficiency" NA                  
#>  [2044] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2047] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2050] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2053] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2056] "no iron deficiency" "no iron deficiency" NA                  
#>  [2059] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2062] "iron deficiency"    "no iron deficiency" NA                  
#>  [2065] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2068] "iron deficiency"    NA                   "iron deficiency"   
#>  [2071] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2074] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2077] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2080] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2083] NA                   "iron deficiency"    "no iron deficiency"
#>  [2086] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2089] "no iron deficiency" NA                   "iron deficiency"   
#>  [2092] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2095] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2098] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2101] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2104] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2107] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2110] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2113] NA                   "no iron deficiency" "no iron deficiency"
#>  [2116] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2119] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2122] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2125] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2128] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2131] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2134] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2137] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2140] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2143] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2146] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2149] "iron deficiency"    "iron deficiency"    NA                  
#>  [2152] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2155] NA                   "iron deficiency"    "no iron deficiency"
#>  [2158] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2161] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2164] "no iron deficiency" "iron deficiency"    NA                  
#>  [2167] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2170] NA                   "iron deficiency"    "iron deficiency"   
#>  [2173] "no iron deficiency" NA                   "iron deficiency"   
#>  [2176] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2179] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2182] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2185] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2188] "iron deficiency"    "no iron deficiency" NA                  
#>  [2191] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2194] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2197] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2200] "no iron deficiency" NA                   "iron deficiency"   
#>  [2203] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2206] NA                   "no iron deficiency" "iron deficiency"   
#>  [2209] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2212] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2215] NA                   "no iron deficiency" "iron deficiency"   
#>  [2218] NA                   NA                   "no iron deficiency"
#>  [2221] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2224] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2227] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2230] "no iron deficiency" "no iron deficiency" NA                  
#>  [2233] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2236] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2239] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2242] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2245] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2248] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2251] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2254] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2257] "no iron deficiency" NA                   NA                  
#>  [2260] "iron deficiency"    NA                   NA                  
#>  [2263] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2266] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2269] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2272] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2275] "iron deficiency"    "iron deficiency"    NA                  
#>  [2278] "no iron deficiency" NA                   "iron deficiency"   
#>  [2281] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2284] "no iron deficiency" "no iron deficiency" NA                  
#>  [2287] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2290] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2293] "no iron deficiency" NA                   "no iron deficiency"
#>  [2296] "iron deficiency"    NA                   "no iron deficiency"
#>  [2299] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2302] "no iron deficiency" "no iron deficiency" NA                  
#>  [2305] "iron deficiency"    "no iron deficiency" NA                  
#>  [2308] "no iron deficiency" "no iron deficiency" NA                  
#>  [2311] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2314] "iron deficiency"    NA                   "no iron deficiency"
#>  [2317] "no iron deficiency" NA                   "no iron deficiency"
#>  [2320] NA                   "iron deficiency"    NA                  
#>  [2323] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2326] "no iron deficiency" NA                   "iron deficiency"   
#>  [2329] NA                   "iron deficiency"    "no iron deficiency"
#>  [2332] "iron deficiency"    "no iron deficiency" NA                  
#>  [2335] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2338] "iron deficiency"    NA                   "no iron deficiency"
#>  [2341] NA                   "no iron deficiency" "no iron deficiency"
#>  [2344] "no iron deficiency" "iron deficiency"    NA                  
#>  [2347] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2350] "iron deficiency"    NA                   "no iron deficiency"
#>  [2353] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2356] "no iron deficiency" "no iron deficiency" NA                  
#>  [2359] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2362] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2365] "no iron deficiency" "iron deficiency"    NA                  
#>  [2368] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2371] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2374] "no iron deficiency" NA                   "iron deficiency"   
#>  [2377] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2380] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2383] NA                   "iron deficiency"    "no iron deficiency"
#>  [2386] NA                   "no iron deficiency" "no iron deficiency"
#>  [2389] NA                   "no iron deficiency" "iron deficiency"   
#>  [2392] "iron deficiency"    NA                   "no iron deficiency"
#>  [2395] "iron deficiency"    "no iron deficiency" NA                  
#>  [2398] NA                   "iron deficiency"    "iron deficiency"   
#>  [2401] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2404] NA                   "iron deficiency"    "no iron deficiency"
#>  [2407] "iron deficiency"    NA                   "iron deficiency"   
#>  [2410] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2413] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2416] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2419] "iron deficiency"    "no iron deficiency" NA                  
#>  [2422] NA                   "iron deficiency"    "no iron deficiency"
#>  [2425] "iron deficiency"    "no iron deficiency" NA                  
#>  [2428] NA                   "no iron deficiency" "no iron deficiency"
#>  [2431] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2434] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2437] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2440] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2443] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2446] NA                   "iron deficiency"    "no iron deficiency"
#>  [2449] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2452] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2455] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2458] NA                   "iron deficiency"    "iron deficiency"   
#>  [2461] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2464] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2467] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2470] NA                   NA                   "iron deficiency"   
#>  [2473] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2476] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2479] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2482] "iron deficiency"    NA                   "iron deficiency"   
#>  [2485] "no iron deficiency" NA                   "no iron deficiency"
#>  [2488] "iron deficiency"    "no iron deficiency" NA                  
#>  [2491] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2494] NA                   NA                   NA                  
#>  [2497] "iron deficiency"    NA                   "iron deficiency"   
#>  [2500] NA                   "iron deficiency"    "iron deficiency"   
#>  [2503] NA                   "iron deficiency"    "no iron deficiency"
#>  [2506] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [2509] NA                   NA                   NA                  
#>  [2512] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2515] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2518] "iron deficiency"    NA                   "iron deficiency"   
#>  [2521] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2524] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2527] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2530] "no iron deficiency" "iron deficiency"    NA                  
#>  [2533] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2536] NA                   "no iron deficiency" "no iron deficiency"
#>  [2539] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2542] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2545] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2548] NA                   "no iron deficiency" "no iron deficiency"
#>  [2551] "no iron deficiency" "no iron deficiency" NA                  
#>  [2554] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2557] "no iron deficiency" NA                   "no iron deficiency"
#>  [2560] "iron deficiency"    "no iron deficiency" NA                  
#>  [2563] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2566] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2569] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2572] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2575] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2578] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2581] NA                   "no iron deficiency" "no iron deficiency"
#>  [2584] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2587] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2590] "no iron deficiency" NA                   "no iron deficiency"
#>  [2593] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2596] NA                   NA                   "no iron deficiency"
#>  [2599] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2602] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2605] "no iron deficiency" "no iron deficiency" NA                  
#>  [2608] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2611] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2614] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2617] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2620] "iron deficiency"    "no iron deficiency" NA                  
#>  [2623] NA                   "no iron deficiency" "no iron deficiency"
#>  [2626] "no iron deficiency" NA                   NA                  
#>  [2629] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2632] "no iron deficiency" NA                   "iron deficiency"   
#>  [2635] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2638] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2641] "no iron deficiency" NA                   "no iron deficiency"
#>  [2644] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2647] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2650] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2653] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2656] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2659] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2662] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2665] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2668] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2671] "no iron deficiency" "no iron deficiency" NA                  
#>  [2674] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2677] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2680] "no iron deficiency" "iron deficiency"    NA                  
#>  [2683] NA                   "no iron deficiency" "no iron deficiency"
#>  [2686] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2689] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2692] NA                   "iron deficiency"    "no iron deficiency"
#>  [2695] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2698] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2701] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2704] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2707] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2710] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2713] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2716] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2719] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2722] NA                   "no iron deficiency" "iron deficiency"   
#>  [2725] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2728] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2731] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2734] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2737] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2740] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2743] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2746] NA                   "no iron deficiency" "no iron deficiency"
#>  [2749] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2752] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2755] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2758] NA                   "no iron deficiency" "no iron deficiency"
#>  [2761] "no iron deficiency" NA                   "no iron deficiency"
#>  [2764] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2767] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2770] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2773] "no iron deficiency" "no iron deficiency" NA                  
#>  [2776] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2779] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2782] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2785] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2788] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2791] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2794] NA                   "iron deficiency"    "no iron deficiency"
#>  [2797] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2800] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2803] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2806] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2809] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2812] NA                   "iron deficiency"    "iron deficiency"   
#>  [2815] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2818] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2821] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2824] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2827] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2830] "no iron deficiency" "no iron deficiency" NA                  
#>  [2833] "no iron deficiency" NA                   "no iron deficiency"
#>  [2836] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [2839] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2842] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2845] NA                   "iron deficiency"    "iron deficiency"   
#>  [2848] "no iron deficiency" NA                   "no iron deficiency"
#>  [2851] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2854] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2857] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2860] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2863] "iron deficiency"    NA                   "no iron deficiency"
#>  [2866] NA                   "no iron deficiency" "no iron deficiency"
#>  [2869] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2872] "no iron deficiency" NA                   NA                  
#>  [2875] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2878] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2881] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2884] "iron deficiency"    "iron deficiency"    NA                  
#>  [2887] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2890] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2893] NA                   "iron deficiency"    "no iron deficiency"
#>  [2896] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2899] "no iron deficiency" "iron deficiency"    NA                  
#>  [2902] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2905] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2908] NA                   "iron deficiency"    "no iron deficiency"
#>  [2911] "iron deficiency"    "no iron deficiency" NA                  
#>  [2914] NA                   NA                   "no iron deficiency"
#>  [2917] "no iron deficiency" "iron deficiency"    NA                  
#>  [2920] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2923] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2926] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2929] "no iron deficiency" NA                   "no iron deficiency"
#>  [2932] "no iron deficiency" NA                   "no iron deficiency"
#>  [2935] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2938] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2941] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2944] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2947] NA                   "no iron deficiency" "iron deficiency"   
#>  [2950] "no iron deficiency" NA                   "no iron deficiency"
#>  [2953] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2956] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2959] "no iron deficiency" "no iron deficiency" NA                  
#>  [2962] "no iron deficiency" NA                   "iron deficiency"   
#>  [2965] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2968] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2971] "no iron deficiency" "no iron deficiency" NA                  
#>  [2974] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2977] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2980] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [2983] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [2986] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [2989] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [2992] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [2995] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [2998] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3001] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3004] "no iron deficiency" "no iron deficiency" NA                  
#>  [3007] NA                   "no iron deficiency" NA                  
#>  [3010] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3013] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3016] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3019] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3022] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3025] "iron deficiency"    NA                   "no iron deficiency"
#>  [3028] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3031] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3034] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3037] NA                   NA                   "no iron deficiency"
#>  [3040] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3043] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3046] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3049] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3052] "no iron deficiency" NA                   "iron deficiency"   
#>  [3055] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3058] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3061] "iron deficiency"    "no iron deficiency" NA                  
#>  [3064] "iron deficiency"    NA                   "no iron deficiency"
#>  [3067] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3070] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3073] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3076] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3079] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3082] NA                   "no iron deficiency" "iron deficiency"   
#>  [3085] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3088] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3091] "no iron deficiency" "no iron deficiency" NA                  
#>  [3094] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3097] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3100] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3103] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3106] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3109] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3112] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3115] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3118] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3121] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3124] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3127] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3130] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3133] "iron deficiency"    "iron deficiency"    NA                  
#>  [3136] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3139] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3142] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3145] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3148] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3151] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3154] "no iron deficiency" "iron deficiency"    NA                  
#>  [3157] "iron deficiency"    "no iron deficiency" NA                  
#>  [3160] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3163] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3166] "no iron deficiency" "no iron deficiency" NA                  
#>  [3169] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3172] NA                   NA                   "no iron deficiency"
#>  [3175] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3178] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3181] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3184] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3187] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3190] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3193] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3196] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3199] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3202] NA                   NA                   "no iron deficiency"
#>  [3205] "iron deficiency"    "iron deficiency"    NA                  
#>  [3208] NA                   "no iron deficiency" "no iron deficiency"
#>  [3211] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3214] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3217] "iron deficiency"    NA                   "iron deficiency"   
#>  [3220] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3223] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3226] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3229] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3232] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3235] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3238] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3241] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3244] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3247] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3250] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3253] NA                   "no iron deficiency" "no iron deficiency"
#>  [3256] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3259] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3262] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3265] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3268] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3271] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3274] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3277] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3280] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3283] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3286] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3289] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3292] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3295] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3298] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3301] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3304] NA                   "no iron deficiency" "no iron deficiency"
#>  [3307] "no iron deficiency" NA                   "no iron deficiency"
#>  [3310] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3313] NA                   "iron deficiency"    "iron deficiency"   
#>  [3316] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3319] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3322] NA                   "no iron deficiency" "no iron deficiency"
#>  [3325] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3328] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3331] "no iron deficiency" NA                   "no iron deficiency"
#>  [3334] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3337] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3340] NA                   "no iron deficiency" "iron deficiency"   
#>  [3343] NA                   "iron deficiency"    "iron deficiency"   
#>  [3346] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3349] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3352] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3355] "no iron deficiency" "iron deficiency"    NA                  
#>  [3358] "no iron deficiency" NA                   "no iron deficiency"
#>  [3361] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3364] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3367] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3370] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3373] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3376] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3379] NA                   "no iron deficiency" "no iron deficiency"
#>  [3382] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3385] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3388] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3391] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3394] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3397] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3400] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3403] "no iron deficiency" NA                   "no iron deficiency"
#>  [3406] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3409] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3412] NA                   "iron deficiency"    "no iron deficiency"
#>  [3415] NA                   "no iron deficiency" "no iron deficiency"
#>  [3418] NA                   "no iron deficiency" "iron deficiency"   
#>  [3421] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3424] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3427] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3430] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3433] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3436] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3439] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3442] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3445] "no iron deficiency" "no iron deficiency" NA                  
#>  [3448] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3451] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3454] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3457] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3460] "no iron deficiency" NA                   "no iron deficiency"
#>  [3463] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3466] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3469] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3472] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3475] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3478] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3481] "no iron deficiency" "iron deficiency"    NA                  
#>  [3484] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3487] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3490] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3493] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3496] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3499] "no iron deficiency" NA                   "no iron deficiency"
#>  [3502] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3505] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3508] "no iron deficiency" "no iron deficiency" NA                  
#>  [3511] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3514] "iron deficiency"    NA                   "no iron deficiency"
#>  [3517] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3520] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3523] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3526] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3529] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3532] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3535] "no iron deficiency" NA                   "no iron deficiency"
#>  [3538] NA                   "iron deficiency"    "iron deficiency"   
#>  [3541] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3544] "iron deficiency"    NA                   "iron deficiency"   
#>  [3547] NA                   "iron deficiency"    "iron deficiency"   
#>  [3550] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3553] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3556] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3559] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3562] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3565] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3568] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3571] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3574] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3577] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3580] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3583] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3586] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3589] NA                   "iron deficiency"    "no iron deficiency"
#>  [3592] "no iron deficiency" NA                   "iron deficiency"   
#>  [3595] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3598] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3601] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3604] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3607] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3610] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3613] NA                   "no iron deficiency" "iron deficiency"   
#>  [3616] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3619] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3622] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3625] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3628] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3631] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3634] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3637] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3640] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3643] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3646] "iron deficiency"    NA                   "iron deficiency"   
#>  [3649] "no iron deficiency" "no iron deficiency" NA                  
#>  [3652] NA                   "no iron deficiency" "no iron deficiency"
#>  [3655] "iron deficiency"    NA                   "no iron deficiency"
#>  [3658] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3661] "no iron deficiency" "no iron deficiency" NA                  
#>  [3664] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3667] "no iron deficiency" NA                   NA                  
#>  [3670] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3673] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3676] "no iron deficiency" NA                   NA                  
#>  [3679] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3682] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3685] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3688] "iron deficiency"    "iron deficiency"    NA                  
#>  [3691] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3694] "iron deficiency"    "no iron deficiency" NA                  
#>  [3697] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3700] NA                   "iron deficiency"    "iron deficiency"   
#>  [3703] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3706] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3709] "no iron deficiency" NA                   NA                  
#>  [3712] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3715] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3718] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3721] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3724] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3727] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3730] "no iron deficiency" "iron deficiency"    NA                  
#>  [3733] NA                   "no iron deficiency" "iron deficiency"   
#>  [3736] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3739] NA                   "no iron deficiency" "no iron deficiency"
#>  [3742] "iron deficiency"    NA                   "no iron deficiency"
#>  [3745] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3748] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3751] NA                   NA                   "iron deficiency"   
#>  [3754] "no iron deficiency" "no iron deficiency" NA                  
#>  [3757] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3760] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3763] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3766] NA                   "iron deficiency"    NA                  
#>  [3769] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3772] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3775] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3778] "iron deficiency"    NA                   "no iron deficiency"
#>  [3781] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3784] NA                   "iron deficiency"    "no iron deficiency"
#>  [3787] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3790] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3793] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3796] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3799] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3802] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3805] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3808] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3811] "iron deficiency"    NA                   "no iron deficiency"
#>  [3814] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3817] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3820] "no iron deficiency" "no iron deficiency" NA                  
#>  [3823] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3826] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3829] "no iron deficiency" "no iron deficiency" NA                  
#>  [3832] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3835] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3838] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3841] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3844] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3847] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3850] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3853] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3856] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3859] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3862] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3865] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3868] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3871] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3874] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3877] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3880] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3883] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3886] "no iron deficiency" NA                   "no iron deficiency"
#>  [3889] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3892] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3895] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3898] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3901] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [3904] NA                   "no iron deficiency" NA                  
#>  [3907] "no iron deficiency" NA                   "iron deficiency"   
#>  [3910] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3913] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3916] "no iron deficiency" "iron deficiency"    NA                  
#>  [3919] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3922] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3925] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3928] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3931] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3934] "iron deficiency"    NA                   "iron deficiency"   
#>  [3937] NA                   "iron deficiency"    "no iron deficiency"
#>  [3940] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3943] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3946] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [3949] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3952] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3955] "no iron deficiency" "iron deficiency"    NA                  
#>  [3958] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3961] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [3964] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [3967] NA                   NA                   "no iron deficiency"
#>  [3970] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [3973] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [3976] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [3979] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [3982] NA                   "no iron deficiency" "no iron deficiency"
#>  [3985] "iron deficiency"    "iron deficiency"    NA                  
#>  [3988] "iron deficiency"    "iron deficiency"    NA                  
#>  [3991] "no iron deficiency" NA                   "iron deficiency"   
#>  [3994] NA                   "no iron deficiency" "iron deficiency"   
#>  [3997] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4000] "no iron deficiency" NA                   "iron deficiency"   
#>  [4003] "iron deficiency"    NA                   "iron deficiency"   
#>  [4006] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4009] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4012] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4015] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4018] NA                   "iron deficiency"    "iron deficiency"   
#>  [4021] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4024] NA                   "no iron deficiency" NA                  
#>  [4027] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4030] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4033] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4036] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4039] NA                   "no iron deficiency" "iron deficiency"   
#>  [4042] NA                   "no iron deficiency" "no iron deficiency"
#>  [4045] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4048] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4051] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4054] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4057] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4060] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4063] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4066] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4069] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4072] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4075] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4078] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4081] NA                   "no iron deficiency" "no iron deficiency"
#>  [4084] "no iron deficiency" NA                   "iron deficiency"   
#>  [4087] "iron deficiency"    "no iron deficiency" NA                  
#>  [4090] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4093] NA                   "iron deficiency"    NA                  
#>  [4096] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4099] "iron deficiency"    "no iron deficiency" NA                  
#>  [4102] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4105] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4108] "no iron deficiency" NA                   "iron deficiency"   
#>  [4111] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4114] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4117] NA                   "iron deficiency"    "iron deficiency"   
#>  [4120] "no iron deficiency" NA                   "no iron deficiency"
#>  [4123] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4126] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4129] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4132] NA                   "no iron deficiency" "iron deficiency"   
#>  [4135] NA                   "no iron deficiency" "no iron deficiency"
#>  [4138] "iron deficiency"    NA                   "iron deficiency"   
#>  [4141] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4144] "iron deficiency"    "no iron deficiency" NA                  
#>  [4147] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4150] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4153] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4156] "no iron deficiency" "no iron deficiency" NA                  
#>  [4159] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4162] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4165] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4168] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4171] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4174] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4177] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4180] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4183] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4186] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4189] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4192] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4195] "no iron deficiency" "no iron deficiency" NA                  
#>  [4198] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4201] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4204] "no iron deficiency" NA                   "iron deficiency"   
#>  [4207] "iron deficiency"    "iron deficiency"    NA                  
#>  [4210] NA                   "iron deficiency"    "no iron deficiency"
#>  [4213] NA                   "iron deficiency"    "iron deficiency"   
#>  [4216] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4219] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4222] NA                   NA                   NA                  
#>  [4225] "iron deficiency"    NA                   "iron deficiency"   
#>  [4228] NA                   "iron deficiency"    "iron deficiency"   
#>  [4231] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4234] NA                   "iron deficiency"    "iron deficiency"   
#>  [4237] NA                   "no iron deficiency" "iron deficiency"   
#>  [4240] "iron deficiency"    "iron deficiency"    NA                  
#>  [4243] NA                   NA                   "no iron deficiency"
#>  [4246] NA                   "iron deficiency"    "iron deficiency"   
#>  [4249] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4252] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4255] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4258] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4261] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4264] "iron deficiency"    NA                   "iron deficiency"   
#>  [4267] NA                   "iron deficiency"    "iron deficiency"   
#>  [4270] NA                   "no iron deficiency" "iron deficiency"   
#>  [4273] NA                   "iron deficiency"    "iron deficiency"   
#>  [4276] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4279] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4282] "iron deficiency"    NA                   "no iron deficiency"
#>  [4285] "iron deficiency"    "iron deficiency"    NA                  
#>  [4288] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4291] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4294] NA                   NA                   "no iron deficiency"
#>  [4297] NA                   NA                   NA                  
#>  [4300] NA                   "iron deficiency"    "iron deficiency"   
#>  [4303] "no iron deficiency" "no iron deficiency" NA                  
#>  [4306] NA                   NA                   NA                  
#>  [4309] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4312] "iron deficiency"    NA                   NA                  
#>  [4315] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4318] NA                   "no iron deficiency" NA                  
#>  [4321] NA                   "iron deficiency"    "no iron deficiency"
#>  [4324] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4327] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4330] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4333] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4336] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4339] "iron deficiency"    NA                   NA                  
#>  [4342] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4345] NA                   "no iron deficiency" "no iron deficiency"
#>  [4348] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4351] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4354] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4357] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4360] NA                   NA                   "iron deficiency"   
#>  [4363] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4366] NA                   "iron deficiency"    "no iron deficiency"
#>  [4369] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4372] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4375] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4378] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4381] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4384] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4387] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4390] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4393] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4396] "no iron deficiency" "no iron deficiency" NA                  
#>  [4399] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4402] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4405] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4408] "no iron deficiency" "iron deficiency"    NA                  
#>  [4411] "no iron deficiency" NA                   "iron deficiency"   
#>  [4414] "iron deficiency"    NA                   "iron deficiency"   
#>  [4417] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4420] "no iron deficiency" NA                   "no iron deficiency"
#>  [4423] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4426] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4429] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4432] NA                   NA                   "no iron deficiency"
#>  [4435] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4438] NA                   "iron deficiency"    "no iron deficiency"
#>  [4441] "no iron deficiency" NA                   "no iron deficiency"
#>  [4444] "iron deficiency"    NA                   "no iron deficiency"
#>  [4447] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4450] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4453] NA                   NA                   "no iron deficiency"
#>  [4456] NA                   "iron deficiency"    "iron deficiency"   
#>  [4459] "iron deficiency"    "no iron deficiency" NA                  
#>  [4462] NA                   "no iron deficiency" "no iron deficiency"
#>  [4465] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4468] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4471] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4474] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4477] NA                   "iron deficiency"    NA                  
#>  [4480] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4483] NA                   "no iron deficiency" "iron deficiency"   
#>  [4486] NA                   "iron deficiency"    "iron deficiency"   
#>  [4489] NA                   "iron deficiency"    "iron deficiency"   
#>  [4492] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4495] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4498] NA                   "no iron deficiency" "iron deficiency"   
#>  [4501] NA                   NA                   "iron deficiency"   
#>  [4504] "no iron deficiency" "no iron deficiency" NA                  
#>  [4507] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4510] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4513] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4516] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4519] NA                   "iron deficiency"    "no iron deficiency"
#>  [4522] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4525] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4528] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4531] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4534] "iron deficiency"    NA                   "iron deficiency"   
#>  [4537] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4540] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4543] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4546] "no iron deficiency" NA                   NA                  
#>  [4549] "no iron deficiency" NA                   NA                  
#>  [4552] "iron deficiency"    NA                   "iron deficiency"   
#>  [4555] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4558] NA                   "iron deficiency"    "no iron deficiency"
#>  [4561] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4564] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4567] NA                   "iron deficiency"    "iron deficiency"   
#>  [4570] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4573] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4576] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4579] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4582] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4585] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4588] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4591] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4594] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4597] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4600] NA                   "iron deficiency"    "iron deficiency"   
#>  [4603] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4606] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4609] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4612] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4615] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4618] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4621] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4624] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4627] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4630] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4633] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4636] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4639] "no iron deficiency" "no iron deficiency" NA                  
#>  [4642] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4645] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4648] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4651] NA                   "iron deficiency"    "iron deficiency"   
#>  [4654] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4657] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4660] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4663] NA                   "iron deficiency"    "iron deficiency"   
#>  [4666] "no iron deficiency" "iron deficiency"    NA                  
#>  [4669] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4672] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4675] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4678] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4681] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4684] NA                   "iron deficiency"    "iron deficiency"   
#>  [4687] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4690] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4693] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4696] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4699] "iron deficiency"    NA                   NA                  
#>  [4702] "iron deficiency"    "no iron deficiency" NA                  
#>  [4705] "no iron deficiency" NA                   "no iron deficiency"
#>  [4708] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4711] "iron deficiency"    NA                   "iron deficiency"   
#>  [4714] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4717] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4720] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4723] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4726] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4729] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4732] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4735] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4738] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4741] NA                   "iron deficiency"    "no iron deficiency"
#>  [4744] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [4747] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4750] NA                   "iron deficiency"    NA                  
#>  [4753] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4756] "iron deficiency"    NA                   "iron deficiency"   
#>  [4759] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4762] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4765] "no iron deficiency" "iron deficiency"    NA                  
#>  [4768] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4771] "no iron deficiency" "no iron deficiency" NA                  
#>  [4774] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4777] "no iron deficiency" NA                   "no iron deficiency"
#>  [4780] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4783] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4786] "iron deficiency"    "no iron deficiency" NA                  
#>  [4789] NA                   "no iron deficiency" "iron deficiency"   
#>  [4792] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4795] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [4798] "iron deficiency"    NA                   "iron deficiency"   
#>  [4801] "no iron deficiency" "iron deficiency"    NA                  
#>  [4804] NA                   "iron deficiency"    "iron deficiency"   
#>  [4807] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4810] "iron deficiency"    NA                   "iron deficiency"   
#>  [4813] NA                   NA                   "iron deficiency"   
#>  [4816] NA                   "iron deficiency"    "iron deficiency"   
#>  [4819] "iron deficiency"    "iron deficiency"    NA                  
#>  [4822] "iron deficiency"    NA                   NA                  
#>  [4825] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4828] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4831] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4834] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4837] "iron deficiency"    "iron deficiency"    NA                  
#>  [4840] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4843] "iron deficiency"    "iron deficiency"    NA                  
#>  [4846] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4849] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4852] NA                   "iron deficiency"    "iron deficiency"   
#>  [4855] NA                   NA                   "iron deficiency"   
#>  [4858] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4861] "no iron deficiency" "iron deficiency"    NA                  
#>  [4864] NA                   "iron deficiency"    "iron deficiency"   
#>  [4867] "iron deficiency"    "iron deficiency"    NA                  
#>  [4870] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4873] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4876] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4879] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4882] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4885] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4888] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4891] "iron deficiency"    NA                   NA                  
#>  [4894] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4897] "iron deficiency"    "no iron deficiency" NA                  
#>  [4900] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4903] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4906] NA                   "iron deficiency"    "iron deficiency"   
#>  [4909] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4912] "iron deficiency"    "iron deficiency"    NA                  
#>  [4915] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4918] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4921] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4924] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [4927] "no iron deficiency" NA                   "no iron deficiency"
#>  [4930] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4933] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4936] "no iron deficiency" NA                   "iron deficiency"   
#>  [4939] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4942] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4945] "no iron deficiency" NA                   "iron deficiency"   
#>  [4948] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4951] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4954] "iron deficiency"    "iron deficiency"    NA                  
#>  [4957] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4960] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4963] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4966] NA                   NA                   "iron deficiency"   
#>  [4969] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4972] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [4975] "iron deficiency"    NA                   "iron deficiency"   
#>  [4978] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [4981] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [4984] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [4987] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4990] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [4993] "iron deficiency"    NA                   "iron deficiency"   
#>  [4996] NA                   "iron deficiency"    "iron deficiency"   
#>  [4999] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5002] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5005] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5008] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5011] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5014] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5017] "no iron deficiency" NA                   "no iron deficiency"
#>  [5020] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5023] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5026] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5029] "no iron deficiency" NA                   "no iron deficiency"
#>  [5032] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5035] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5038] "no iron deficiency" "no iron deficiency" NA                  
#>  [5041] "iron deficiency"    "iron deficiency"    NA                  
#>  [5044] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5047] NA                   "no iron deficiency" "no iron deficiency"
#>  [5050] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5053] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5056] NA                   "no iron deficiency" "iron deficiency"   
#>  [5059] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5062] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5065] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5068] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5071] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5074] "iron deficiency"    "iron deficiency"    NA                  
#>  [5077] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5080] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5083] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5086] "iron deficiency"    "no iron deficiency" NA                  
#>  [5089] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5092] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5095] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5098] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5101] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5104] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5107] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5110] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5113] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5116] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5119] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5122] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5125] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5128] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5131] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5134] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5137] NA                   "iron deficiency"    "iron deficiency"   
#>  [5140] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5143] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5146] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5149] NA                   "no iron deficiency" "iron deficiency"   
#>  [5152] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5155] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5158] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5161] "iron deficiency"    NA                   NA                  
#>  [5164] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5167] NA                   "no iron deficiency" NA                  
#>  [5170] "iron deficiency"    NA                   "no iron deficiency"
#>  [5173] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5176] NA                   "no iron deficiency" "no iron deficiency"
#>  [5179] NA                   "no iron deficiency" "iron deficiency"   
#>  [5182] "no iron deficiency" NA                   "no iron deficiency"
#>  [5185] "no iron deficiency" "iron deficiency"    NA                  
#>  [5188] NA                   "no iron deficiency" NA                  
#>  [5191] NA                   "no iron deficiency" "iron deficiency"   
#>  [5194] "iron deficiency"    NA                   NA                  
#>  [5197] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5200] "no iron deficiency" NA                   "iron deficiency"   
#>  [5203] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5206] "iron deficiency"    NA                   NA                  
#>  [5209] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5212] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5215] "iron deficiency"    "iron deficiency"    NA                  
#>  [5218] NA                   NA                   "iron deficiency"   
#>  [5221] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5224] "no iron deficiency" "no iron deficiency" NA                  
#>  [5227] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5230] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5233] NA                   "no iron deficiency" "iron deficiency"   
#>  [5236] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5239] "no iron deficiency" NA                   "no iron deficiency"
#>  [5242] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5245] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5248] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5251] NA                   NA                   "iron deficiency"   
#>  [5254] "iron deficiency"    NA                   "iron deficiency"   
#>  [5257] NA                   "iron deficiency"    "iron deficiency"   
#>  [5260] NA                   "iron deficiency"    "no iron deficiency"
#>  [5263] "iron deficiency"    "iron deficiency"    NA                  
#>  [5266] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5269] NA                   "no iron deficiency" "iron deficiency"   
#>  [5272] NA                   "iron deficiency"    "iron deficiency"   
#>  [5275] "iron deficiency"    NA                   "no iron deficiency"
#>  [5278] NA                   "iron deficiency"    "iron deficiency"   
#>  [5281] NA                   "iron deficiency"    NA                  
#>  [5284] "no iron deficiency" NA                   NA                  
#>  [5287] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5290] "iron deficiency"    NA                   NA                  
#>  [5293] NA                   NA                   NA                  
#>  [5296] "no iron deficiency" "no iron deficiency" NA                  
#>  [5299] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5302] NA                   NA                   NA                  
#>  [5305] NA                   "iron deficiency"    "iron deficiency"   
#>  [5308] NA                   "iron deficiency"    NA                  
#>  [5311] "no iron deficiency" "no iron deficiency" NA                  
#>  [5314] NA                   "iron deficiency"    NA                  
#>  [5317] "iron deficiency"    "iron deficiency"    NA                  
#>  [5320] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5323] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5326] "iron deficiency"    NA                   NA                  
#>  [5329] "iron deficiency"    "iron deficiency"    NA                  
#>  [5332] NA                   NA                   NA                  
#>  [5335] NA                   "iron deficiency"    "iron deficiency"   
#>  [5338] NA                   "iron deficiency"    NA                  
#>  [5341] NA                   NA                   NA                  
#>  [5344] NA                   "iron deficiency"    NA                  
#>  [5347] NA                   "iron deficiency"    "iron deficiency"   
#>  [5350] NA                   "no iron deficiency" NA                  
#>  [5353] NA                   "iron deficiency"    NA                  
#>  [5356] "iron deficiency"    NA                   NA                  
#>  [5359] NA                   "iron deficiency"    NA                  
#>  [5362] NA                   "no iron deficiency" NA                  
#>  [5365] "no iron deficiency" NA                   "iron deficiency"   
#>  [5368] "no iron deficiency" "no iron deficiency" NA                  
#>  [5371] "no iron deficiency" NA                   "iron deficiency"   
#>  [5374] "iron deficiency"    "iron deficiency"    NA                  
#>  [5377] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5380] NA                   NA                   NA                  
#>  [5383] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5386] "iron deficiency"    NA                   NA                  
#>  [5389] NA                   "no iron deficiency" "iron deficiency"   
#>  [5392] NA                   NA                   NA                  
#>  [5395] "iron deficiency"    "iron deficiency"    NA                  
#>  [5398] NA                   "iron deficiency"    NA                  
#>  [5401] "iron deficiency"    "iron deficiency"    NA                  
#>  [5404] NA                   "iron deficiency"    NA                  
#>  [5407] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5410] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5413] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5416] "iron deficiency"    NA                   NA                  
#>  [5419] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5422] "iron deficiency"    "no iron deficiency" NA                  
#>  [5425] "no iron deficiency" "no iron deficiency" NA                  
#>  [5428] NA                   "no iron deficiency" "no iron deficiency"
#>  [5431] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5434] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5437] NA                   "no iron deficiency" "iron deficiency"   
#>  [5440] NA                   "iron deficiency"    "iron deficiency"   
#>  [5443] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5446] NA                   "iron deficiency"    NA                  
#>  [5449] "iron deficiency"    NA                   "no iron deficiency"
#>  [5452] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5455] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5458] NA                   "iron deficiency"    NA                  
#>  [5461] NA                   "iron deficiency"    "no iron deficiency"
#>  [5464] "iron deficiency"    "iron deficiency"    NA                  
#>  [5467] NA                   NA                   "iron deficiency"   
#>  [5470] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5473] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5476] "iron deficiency"    "iron deficiency"    NA                  
#>  [5479] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5482] NA                   "iron deficiency"    NA                  
#>  [5485] "iron deficiency"    "iron deficiency"    NA                  
#>  [5488] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5491] "iron deficiency"    NA                   "no iron deficiency"
#>  [5494] NA                   "iron deficiency"    "iron deficiency"   
#>  [5497] "iron deficiency"    "iron deficiency"    NA                  
#>  [5500] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5503] NA                   "iron deficiency"    "no iron deficiency"
#>  [5506] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5509] NA                   "iron deficiency"    "iron deficiency"   
#>  [5512] "iron deficiency"    "iron deficiency"    NA                  
#>  [5515] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5518] "iron deficiency"    "iron deficiency"    NA                  
#>  [5521] "iron deficiency"    NA                   NA                  
#>  [5524] "iron deficiency"    NA                   "iron deficiency"   
#>  [5527] NA                   "iron deficiency"    "iron deficiency"   
#>  [5530] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5533] "iron deficiency"    NA                   NA                  
#>  [5536] "no iron deficiency" NA                   "iron deficiency"   
#>  [5539] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5542] "iron deficiency"    NA                   "iron deficiency"   
#>  [5545] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5548] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5551] "iron deficiency"    "iron deficiency"    NA                  
#>  [5554] NA                   "iron deficiency"    "iron deficiency"   
#>  [5557] "iron deficiency"    NA                   "iron deficiency"   
#>  [5560] "no iron deficiency" NA                   "iron deficiency"   
#>  [5563] NA                   "iron deficiency"    "iron deficiency"   
#>  [5566] "no iron deficiency" NA                   "no iron deficiency"
#>  [5569] "iron deficiency"    "no iron deficiency" NA                  
#>  [5572] "no iron deficiency" NA                   "iron deficiency"   
#>  [5575] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5578] NA                   NA                   "iron deficiency"   
#>  [5581] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5584] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5587] "iron deficiency"    "iron deficiency"    NA                  
#>  [5590] NA                   "iron deficiency"    NA                  
#>  [5593] "iron deficiency"    "iron deficiency"    NA                  
#>  [5596] NA                   "no iron deficiency" "no iron deficiency"
#>  [5599] "iron deficiency"    "iron deficiency"    NA                  
#>  [5602] NA                   "iron deficiency"    "no iron deficiency"
#>  [5605] "iron deficiency"    "iron deficiency"    NA                  
#>  [5608] "iron deficiency"    "no iron deficiency" NA                  
#>  [5611] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5614] NA                   NA                   "iron deficiency"   
#>  [5617] NA                   "iron deficiency"    "iron deficiency"   
#>  [5620] "no iron deficiency" NA                   "iron deficiency"   
#>  [5623] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5626] "no iron deficiency" "no iron deficiency" NA                  
#>  [5629] "iron deficiency"    NA                   "iron deficiency"   
#>  [5632] "iron deficiency"    NA                   "iron deficiency"   
#>  [5635] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5638] "no iron deficiency" NA                   "no iron deficiency"
#>  [5641] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5644] NA                   NA                   NA                  
#>  [5647] "iron deficiency"    NA                   "iron deficiency"   
#>  [5650] "iron deficiency"    NA                   "iron deficiency"   
#>  [5653] NA                   "iron deficiency"    "no iron deficiency"
#>  [5656] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5659] "iron deficiency"    "iron deficiency"    NA                  
#>  [5662] "iron deficiency"    "iron deficiency"    NA                  
#>  [5665] "iron deficiency"    NA                   "iron deficiency"   
#>  [5668] NA                   "no iron deficiency" "iron deficiency"   
#>  [5671] NA                   "no iron deficiency" "iron deficiency"   
#>  [5674] "iron deficiency"    "iron deficiency"    NA                  
#>  [5677] NA                   "iron deficiency"    NA                  
#>  [5680] "no iron deficiency" "iron deficiency"    NA                  
#>  [5683] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5686] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5689] NA                   NA                   "iron deficiency"   
#>  [5692] "iron deficiency"    NA                   "iron deficiency"   
#>  [5695] NA                   NA                   "no iron deficiency"
#>  [5698] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5701] NA                   "no iron deficiency" "no iron deficiency"
#>  [5704] NA                   "no iron deficiency" NA                  
#>  [5707] NA                   "iron deficiency"    "no iron deficiency"
#>  [5710] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5713] NA                   "no iron deficiency" "no iron deficiency"
#>  [5716] "no iron deficiency" NA                   "iron deficiency"   
#>  [5719] "iron deficiency"    NA                   "iron deficiency"   
#>  [5722] NA                   NA                   "iron deficiency"   
#>  [5725] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5728] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5731] NA                   "iron deficiency"    "iron deficiency"   
#>  [5734] "iron deficiency"    "no iron deficiency" NA                  
#>  [5737] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5740] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5743] "iron deficiency"    NA                   "iron deficiency"   
#>  [5746] "iron deficiency"    NA                   "no iron deficiency"
#>  [5749] "no iron deficiency" "no iron deficiency" NA                  
#>  [5752] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5755] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5758] NA                   NA                   "iron deficiency"   
#>  [5761] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5764] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5767] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5770] "no iron deficiency" NA                   NA                  
#>  [5773] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5776] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5779] NA                   "iron deficiency"    "iron deficiency"   
#>  [5782] "iron deficiency"    "iron deficiency"    NA                  
#>  [5785] NA                   "iron deficiency"    "iron deficiency"   
#>  [5788] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5791] NA                   NA                   "iron deficiency"   
#>  [5794] NA                   "iron deficiency"    NA                  
#>  [5797] NA                   "no iron deficiency" NA                  
#>  [5800] NA                   "no iron deficiency" "iron deficiency"   
#>  [5803] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5806] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5809] NA                   NA                   NA                  
#>  [5812] NA                   "no iron deficiency" "no iron deficiency"
#>  [5815] "iron deficiency"    "no iron deficiency" NA                  
#>  [5818] "iron deficiency"    "no iron deficiency" NA                  
#>  [5821] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5824] "iron deficiency"    "iron deficiency"    NA                  
#>  [5827] NA                   NA                   NA                  
#>  [5830] NA                   NA                   NA                  
#>  [5833] NA                   NA                   NA                  
#>  [5836] NA                   NA                   NA                  
#>  [5839] NA                   NA                   NA                  
#>  [5842] NA                   NA                   NA                  
#>  [5845] NA                   NA                   NA                  
#>  [5848] NA                   NA                   NA                  
#>  [5851] NA                   NA                   NA                  
#>  [5854] NA                   NA                   NA                  
#>  [5857] NA                   NA                   NA                  
#>  [5860] NA                   NA                   NA                  
#>  [5863] NA                   NA                   NA                  
#>  [5866] NA                   "iron deficiency"    "iron deficiency"   
#>  [5869] NA                   "no iron deficiency" "no iron deficiency"
#>  [5872] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5875] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5878] "iron deficiency"    "iron deficiency"    NA                  
#>  [5881] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [5884] "iron deficiency"    NA                   "no iron deficiency"
#>  [5887] "no iron deficiency" "iron deficiency"    NA                  
#>  [5890] "iron deficiency"    "iron deficiency"    NA                  
#>  [5893] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5896] "iron deficiency"    "iron deficiency"    NA                  
#>  [5899] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5902] NA                   "iron deficiency"    NA                  
#>  [5905] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5908] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5911] NA                   NA                   NA                  
#>  [5914] "no iron deficiency" NA                   "iron deficiency"   
#>  [5917] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5920] "no iron deficiency" "no iron deficiency" NA                  
#>  [5923] NA                   "iron deficiency"    "iron deficiency"   
#>  [5926] NA                   "iron deficiency"    "no iron deficiency"
#>  [5929] "no iron deficiency" NA                   "iron deficiency"   
#>  [5932] "iron deficiency"    NA                   NA                  
#>  [5935] "iron deficiency"    NA                   NA                  
#>  [5938] NA                   NA                   "iron deficiency"   
#>  [5941] "no iron deficiency" NA                   NA                  
#>  [5944] "iron deficiency"    "no iron deficiency" NA                  
#>  [5947] "iron deficiency"    NA                   NA                  
#>  [5950] NA                   NA                   "iron deficiency"   
#>  [5953] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [5956] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [5959] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [5962] "no iron deficiency" "iron deficiency"    NA                  
#>  [5965] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [5968] "iron deficiency"    "iron deficiency"    NA                  
#>  [5971] "no iron deficiency" NA                   "no iron deficiency"
#>  [5974] NA                   "iron deficiency"    "no iron deficiency"
#>  [5977] "no iron deficiency" "iron deficiency"    NA                  
#>  [5980] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [5983] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5986] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5989] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [5992] "iron deficiency"    NA                   "iron deficiency"   
#>  [5995] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [5998] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6001] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6004] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6007] NA                   "no iron deficiency" "no iron deficiency"
#>  [6010] "iron deficiency"    NA                   NA                  
#>  [6013] "iron deficiency"    "iron deficiency"    NA                  
#>  [6016] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6019] NA                   NA                   "iron deficiency"   
#>  [6022] "iron deficiency"    "iron deficiency"    NA                  
#>  [6025] "iron deficiency"    "no iron deficiency" NA                  
#>  [6028] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6031] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6034] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6037] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6040] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6043] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6046] "iron deficiency"    "iron deficiency"    NA                  
#>  [6049] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6052] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6055] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6058] NA                   "iron deficiency"    "iron deficiency"   
#>  [6061] "iron deficiency"    NA                   "no iron deficiency"
#>  [6064] "iron deficiency"    "iron deficiency"    NA                  
#>  [6067] "no iron deficiency" NA                   "no iron deficiency"
#>  [6070] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6073] NA                   "iron deficiency"    "iron deficiency"   
#>  [6076] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6079] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6082] NA                   "iron deficiency"    "iron deficiency"   
#>  [6085] NA                   "no iron deficiency" "no iron deficiency"
#>  [6088] "no iron deficiency" "iron deficiency"    NA                  
#>  [6091] NA                   "no iron deficiency" "iron deficiency"   
#>  [6094] NA                   NA                   "no iron deficiency"
#>  [6097] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6100] "no iron deficiency" NA                   "no iron deficiency"
#>  [6103] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6106] NA                   NA                   "no iron deficiency"
#>  [6109] NA                   "no iron deficiency" "iron deficiency"   
#>  [6112] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6115] "no iron deficiency" "no iron deficiency" NA                  
#>  [6118] NA                   NA                   "no iron deficiency"
#>  [6121] NA                   "no iron deficiency" "iron deficiency"   
#>  [6124] "iron deficiency"    NA                   "no iron deficiency"
#>  [6127] "no iron deficiency" NA                   "iron deficiency"   
#>  [6130] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6133] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6136] "no iron deficiency" NA                   "iron deficiency"   
#>  [6139] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6142] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6145] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6148] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6151] "iron deficiency"    NA                   "no iron deficiency"
#>  [6154] NA                   "iron deficiency"    NA                  
#>  [6157] "no iron deficiency" "iron deficiency"    NA                  
#>  [6160] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6163] NA                   NA                   NA                  
#>  [6166] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6169] "iron deficiency"    NA                   "iron deficiency"   
#>  [6172] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6175] "iron deficiency"    "no iron deficiency" NA                  
#>  [6178] NA                   "iron deficiency"    "iron deficiency"   
#>  [6181] NA                   "no iron deficiency" "iron deficiency"   
#>  [6184] NA                   "iron deficiency"    "no iron deficiency"
#>  [6187] "iron deficiency"    NA                   "no iron deficiency"
#>  [6190] "iron deficiency"    "no iron deficiency" NA                  
#>  [6193] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6196] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6199] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6202] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6205] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6208] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6211] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6214] NA                   NA                   NA                  
#>  [6217] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6220] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6223] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6226] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6229] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6232] "iron deficiency"    "iron deficiency"    NA                  
#>  [6235] NA                   NA                   NA                  
#>  [6238] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6241] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6244] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6247] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6250] NA                   "iron deficiency"    "iron deficiency"   
#>  [6253] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6256] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6259] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6262] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6265] "no iron deficiency" NA                   "iron deficiency"   
#>  [6268] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6271] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6274] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6277] NA                   "iron deficiency"    "iron deficiency"   
#>  [6280] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6283] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6286] NA                   "iron deficiency"    "no iron deficiency"
#>  [6289] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6292] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6295] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6298] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6301] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6304] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6307] NA                   "no iron deficiency" "no iron deficiency"
#>  [6310] "iron deficiency"    NA                   "no iron deficiency"
#>  [6313] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6316] "iron deficiency"    NA                   "no iron deficiency"
#>  [6319] "no iron deficiency" NA                   NA                  
#>  [6322] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6325] "iron deficiency"    NA                   "iron deficiency"   
#>  [6328] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6331] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6334] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6337] "no iron deficiency" NA                   "no iron deficiency"
#>  [6340] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6343] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6346] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6349] NA                   "no iron deficiency" "iron deficiency"   
#>  [6352] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6355] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6358] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6361] "iron deficiency"    NA                   "iron deficiency"   
#>  [6364] "iron deficiency"    "iron deficiency"    NA                  
#>  [6367] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6370] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6373] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6376] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6379] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6382] "iron deficiency"    "iron deficiency"    NA                  
#>  [6385] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6388] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6391] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6394] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6397] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6400] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6403] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6406] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6409] NA                   "iron deficiency"    "iron deficiency"   
#>  [6412] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6415] "no iron deficiency" NA                   "iron deficiency"   
#>  [6418] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6421] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6424] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6427] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6430] "no iron deficiency" "iron deficiency"    NA                  
#>  [6433] "iron deficiency"    "no iron deficiency" NA                  
#>  [6436] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6439] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6442] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6445] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6448] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6451] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6454] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6457] "iron deficiency"    NA                   "iron deficiency"   
#>  [6460] "no iron deficiency" NA                   "no iron deficiency"
#>  [6463] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6466] "no iron deficiency" "no iron deficiency" NA                  
#>  [6469] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6472] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6475] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6478] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6481] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6484] NA                   "iron deficiency"    "iron deficiency"   
#>  [6487] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6490] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6493] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6496] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6499] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6502] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6505] "no iron deficiency" "iron deficiency"    NA                  
#>  [6508] NA                   "iron deficiency"    "iron deficiency"   
#>  [6511] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6514] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6517] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6520] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6523] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6526] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6529] NA                   "iron deficiency"    "iron deficiency"   
#>  [6532] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6535] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6538] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6541] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6544] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6547] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6550] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6553] "iron deficiency"    NA                   NA                  
#>  [6556] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6559] NA                   NA                   "no iron deficiency"
#>  [6562] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6565] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6568] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6571] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6574] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6577] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6580] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6583] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6586] "no iron deficiency" NA                   "iron deficiency"   
#>  [6589] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6592] "iron deficiency"    NA                   "no iron deficiency"
#>  [6595] "no iron deficiency" NA                   "iron deficiency"   
#>  [6598] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6601] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6604] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6607] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6610] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6613] "iron deficiency"    NA                   "iron deficiency"   
#>  [6616] NA                   NA                   "iron deficiency"   
#>  [6619] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6622] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6625] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6628] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6631] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6634] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6637] NA                   "no iron deficiency" "no iron deficiency"
#>  [6640] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6643] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6646] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6649] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6652] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6655] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6658] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6661] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6664] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6667] NA                   "no iron deficiency" "no iron deficiency"
#>  [6670] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6673] "no iron deficiency" NA                   "no iron deficiency"
#>  [6676] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6679] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6682] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6685] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6688] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6691] "no iron deficiency" "no iron deficiency" NA                  
#>  [6694] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6697] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6700] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6703] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6706] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6709] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6712] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6715] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6718] "no iron deficiency" "no iron deficiency" NA                  
#>  [6721] "no iron deficiency" NA                   "no iron deficiency"
#>  [6724] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6727] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6730] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6733] "iron deficiency"    "no iron deficiency" NA                  
#>  [6736] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6739] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6742] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6745] NA                   "no iron deficiency" "no iron deficiency"
#>  [6748] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6751] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6754] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6757] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6760] "iron deficiency"    NA                   "no iron deficiency"
#>  [6763] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6766] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6769] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6772] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6775] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6778] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6781] NA                   "no iron deficiency" "no iron deficiency"
#>  [6784] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6787] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6790] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6793] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6796] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6799] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6802] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6805] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6808] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6811] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6814] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6817] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6820] "iron deficiency"    "iron deficiency"    NA                  
#>  [6823] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6826] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6829] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6832] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6835] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6838] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6841] NA                   "no iron deficiency" "no iron deficiency"
#>  [6844] NA                   "no iron deficiency" "iron deficiency"   
#>  [6847] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6850] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6853] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6856] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6859] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6862] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6865] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6868] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6871] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6874] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6877] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6880] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6883] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6886] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6889] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6892] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6895] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6898] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6901] "no iron deficiency" NA                   "no iron deficiency"
#>  [6904] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6907] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6910] "no iron deficiency" "no iron deficiency" NA                  
#>  [6913] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6916] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6919] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [6922] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6925] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6928] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [6931] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6934] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6937] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6940] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6943] NA                   "no iron deficiency" NA                  
#>  [6946] NA                   "no iron deficiency" "no iron deficiency"
#>  [6949] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6952] "no iron deficiency" "no iron deficiency" NA                  
#>  [6955] NA                   "no iron deficiency" "no iron deficiency"
#>  [6958] "iron deficiency"    "no iron deficiency" NA                  
#>  [6961] "no iron deficiency" NA                   "no iron deficiency"
#>  [6964] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [6967] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6970] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6973] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6976] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6979] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6982] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [6985] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [6988] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [6991] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [6994] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [6997] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7000] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7003] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7006] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7009] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7012] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7015] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7018] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7021] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7024] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7027] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7030] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7033] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7036] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7039] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7042] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7045] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7048] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7051] "no iron deficiency" NA                   "no iron deficiency"
#>  [7054] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7057] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7060] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7063] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7066] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7069] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7072] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7075] NA                   "iron deficiency"    "no iron deficiency"
#>  [7078] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7081] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7084] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7087] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7090] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7093] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7096] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7099] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7102] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7105] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7108] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7111] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7114] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7117] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7120] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7123] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7126] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7129] "no iron deficiency" "no iron deficiency" NA                  
#>  [7132] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7135] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7138] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7141] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7144] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7147] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7150] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7153] NA                   "no iron deficiency" "no iron deficiency"
#>  [7156] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7159] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7162] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7165] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7168] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7171] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7174] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7177] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7180] "iron deficiency"    "no iron deficiency" NA                  
#>  [7183] "no iron deficiency" NA                   "no iron deficiency"
#>  [7186] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7189] NA                   "iron deficiency"    "no iron deficiency"
#>  [7192] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7195] NA                   "no iron deficiency" "no iron deficiency"
#>  [7198] "no iron deficiency" NA                   "iron deficiency"   
#>  [7201] NA                   "iron deficiency"    "no iron deficiency"
#>  [7204] "iron deficiency"    "iron deficiency"    NA                  
#>  [7207] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7210] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7213] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7216] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7219] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7222] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7225] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7228] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7231] NA                   "iron deficiency"    "no iron deficiency"
#>  [7234] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7237] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7240] "iron deficiency"    NA                   "no iron deficiency"
#>  [7243] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7246] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7249] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7252] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7255] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7258] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7261] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7264] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7267] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7270] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7273] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7276] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7279] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7282] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7285] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7288] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7291] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7294] NA                   "iron deficiency"    "no iron deficiency"
#>  [7297] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7300] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7303] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7306] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7309] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7312] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7315] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7318] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7321] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7324] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7327] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7330] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7333] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7336] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7339] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7342] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7345] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7348] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7351] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7354] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7357] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7360] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7363] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7366] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7369] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7372] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7375] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7378] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7381] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7384] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7387] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7390] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7393] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7396] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7399] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7402] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7405] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7408] "iron deficiency"    NA                   "no iron deficiency"
#>  [7411] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7414] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7417] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7420] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7423] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7426] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7429] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7432] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7435] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7438] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7441] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7444] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7447] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7450] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7453] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7456] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7459] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7462] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7465] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7468] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7471] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7474] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7477] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7480] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7483] "no iron deficiency" NA                   "iron deficiency"   
#>  [7486] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7489] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7492] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7495] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7498] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7501] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7504] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7507] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7510] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7513] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7516] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7519] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7522] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7525] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7528] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7531] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7534] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7537] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7540] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7543] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7546] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7549] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7552] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7555] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7558] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7561] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7564] NA                   "iron deficiency"    "iron deficiency"   
#>  [7567] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7570] "no iron deficiency" "iron deficiency"    NA                  
#>  [7573] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7576] "no iron deficiency" NA                   "iron deficiency"   
#>  [7579] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7582] "no iron deficiency" NA                   "iron deficiency"   
#>  [7585] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7588] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7591] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7594] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7597] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7600] "no iron deficiency" "no iron deficiency" NA                  
#>  [7603] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7606] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7609] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7612] NA                   "iron deficiency"    "no iron deficiency"
#>  [7615] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7618] "iron deficiency"    "no iron deficiency" NA                  
#>  [7621] NA                   "no iron deficiency" NA                  
#>  [7624] NA                   "no iron deficiency" "iron deficiency"   
#>  [7627] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7630] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7633] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7636] NA                   "no iron deficiency" NA                  
#>  [7639] "no iron deficiency" NA                   NA                  
#>  [7642] "no iron deficiency" NA                   NA                  
#>  [7645] "iron deficiency"    NA                   NA                  
#>  [7648] NA                   NA                   NA                  
#>  [7651] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [7654] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7657] NA                   NA                   NA                  
#>  [7660] NA                   "iron deficiency"    NA                  
#>  [7663] NA                   NA                   "no iron deficiency"
#>  [7666] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7669] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7672] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7675] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7678] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7681] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7684] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7687] "no iron deficiency" "iron deficiency"    NA                  
#>  [7690] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7693] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7696] NA                   "iron deficiency"    "no iron deficiency"
#>  [7699] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7702] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7705] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7708] NA                   "no iron deficiency" "no iron deficiency"
#>  [7711] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7714] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7717] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7720] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7723] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7726] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7729] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7732] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7735] NA                   "no iron deficiency" "no iron deficiency"
#>  [7738] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7741] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7744] "no iron deficiency" NA                   "iron deficiency"   
#>  [7747] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7750] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7753] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7756] NA                   "no iron deficiency" "iron deficiency"   
#>  [7759] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7762] NA                   NA                   "iron deficiency"   
#>  [7765] "no iron deficiency" "no iron deficiency" NA                  
#>  [7768] "no iron deficiency" NA                   "iron deficiency"   
#>  [7771] "no iron deficiency" NA                   NA                  
#>  [7774] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7777] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7780] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7783] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7786] NA                   "no iron deficiency" NA                  
#>  [7789] "iron deficiency"    "no iron deficiency" NA                  
#>  [7792] NA                   "iron deficiency"    NA                  
#>  [7795] "no iron deficiency" "iron deficiency"    NA                  
#>  [7798] "no iron deficiency" "no iron deficiency" NA                  
#>  [7801] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7804] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7807] "iron deficiency"    "iron deficiency"    NA                  
#>  [7810] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7813] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7816] "iron deficiency"    NA                   "iron deficiency"   
#>  [7819] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7822] "no iron deficiency" "iron deficiency"    NA                  
#>  [7825] NA                   "no iron deficiency" "iron deficiency"   
#>  [7828] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7831] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7834] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7837] "no iron deficiency" NA                   "no iron deficiency"
#>  [7840] "no iron deficiency" NA                   "iron deficiency"   
#>  [7843] NA                   "no iron deficiency" NA                  
#>  [7846] "no iron deficiency" NA                   "no iron deficiency"
#>  [7849] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7852] NA                   "no iron deficiency" "no iron deficiency"
#>  [7855] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7858] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7861] NA                   "no iron deficiency" "iron deficiency"   
#>  [7864] "no iron deficiency" "iron deficiency"    NA                  
#>  [7867] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7870] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7873] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7876] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7879] NA                   "no iron deficiency" "no iron deficiency"
#>  [7882] "iron deficiency"    "iron deficiency"    NA                  
#>  [7885] "iron deficiency"    NA                   "no iron deficiency"
#>  [7888] NA                   "no iron deficiency" "iron deficiency"   
#>  [7891] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7894] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7897] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7900] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7903] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7906] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7909] "no iron deficiency" "iron deficiency"    NA                  
#>  [7912] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7915] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7918] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7921] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7924] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [7927] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [7930] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [7933] NA                   NA                   "iron deficiency"   
#>  [7936] "iron deficiency"    NA                   "iron deficiency"   
#>  [7939] "iron deficiency"    NA                   "iron deficiency"   
#>  [7942] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7945] "iron deficiency"    NA                   "iron deficiency"   
#>  [7948] "iron deficiency"    NA                   "iron deficiency"   
#>  [7951] "no iron deficiency" NA                   "no iron deficiency"
#>  [7954] "iron deficiency"    NA                   NA                  
#>  [7957] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [7960] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7963] "iron deficiency"    NA                   "no iron deficiency"
#>  [7966] NA                   NA                   NA                  
#>  [7969] NA                   NA                   "iron deficiency"   
#>  [7972] NA                   NA                   "iron deficiency"   
#>  [7975] "no iron deficiency" NA                   "iron deficiency"   
#>  [7978] NA                   "iron deficiency"    "iron deficiency"   
#>  [7981] "no iron deficiency" NA                   NA                  
#>  [7984] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7987] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7990] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [7993] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [7996] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [7999] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8002] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8005] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8008] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8011] "no iron deficiency" NA                   "iron deficiency"   
#>  [8014] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8017] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8020] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8023] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8026] "iron deficiency"    NA                   NA                  
#>  [8029] NA                   "no iron deficiency" "no iron deficiency"
#>  [8032] NA                   "no iron deficiency" "no iron deficiency"
#>  [8035] "no iron deficiency" "no iron deficiency" NA                  
#>  [8038] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8041] NA                   "no iron deficiency" "no iron deficiency"
#>  [8044] NA                   "iron deficiency"    "iron deficiency"   
#>  [8047] NA                   "no iron deficiency" NA                  
#>  [8050] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8053] "iron deficiency"    "no iron deficiency" NA                  
#>  [8056] "iron deficiency"    NA                   NA                  
#>  [8059] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8062] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8065] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8068] "iron deficiency"    "no iron deficiency" NA                  
#>  [8071] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8074] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8077] NA                   NA                   "no iron deficiency"
#>  [8080] "no iron deficiency" "no iron deficiency" NA                  
#>  [8083] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8086] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8089] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8092] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8095] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8098] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8101] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8104] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8107] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8110] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8113] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8116] NA                   "iron deficiency"    "iron deficiency"   
#>  [8119] NA                   NA                   NA                  
#>  [8122] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8125] NA                   "iron deficiency"    "no iron deficiency"
#>  [8128] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8131] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8134] NA                   "iron deficiency"    "iron deficiency"   
#>  [8137] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8140] NA                   "iron deficiency"    "iron deficiency"   
#>  [8143] "iron deficiency"    NA                   "iron deficiency"   
#>  [8146] NA                   NA                   "iron deficiency"   
#>  [8149] "iron deficiency"    "no iron deficiency" NA                  
#>  [8152] "no iron deficiency" NA                   "no iron deficiency"
#>  [8155] NA                   "iron deficiency"    NA                  
#>  [8158] "iron deficiency"    NA                   "no iron deficiency"
#>  [8161] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8164] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8167] NA                   "no iron deficiency" "no iron deficiency"
#>  [8170] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8173] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8176] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8179] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8182] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8185] "no iron deficiency" NA                   NA                  
#>  [8188] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8191] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8194] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8197] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8200] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8203] NA                   "no iron deficiency" "no iron deficiency"
#>  [8206] "no iron deficiency" "no iron deficiency" NA                  
#>  [8209] NA                   "no iron deficiency" "iron deficiency"   
#>  [8212] NA                   NA                   "iron deficiency"   
#>  [8215] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8218] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8221] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8224] "no iron deficiency" "no iron deficiency" NA                  
#>  [8227] "no iron deficiency" "no iron deficiency" NA                  
#>  [8230] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8233] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8236] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8239] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8242] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8245] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8248] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8251] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8254] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8257] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8260] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8263] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8266] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8269] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8272] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8275] "no iron deficiency" NA                   "no iron deficiency"
#>  [8278] "iron deficiency"    "no iron deficiency" NA                  
#>  [8281] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8284] NA                   "no iron deficiency" "iron deficiency"   
#>  [8287] NA                   "no iron deficiency" "no iron deficiency"
#>  [8290] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8293] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8296] "no iron deficiency" "no iron deficiency" NA                  
#>  [8299] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8302] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8305] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8308] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8311] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8314] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8317] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8320] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8323] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8326] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8329] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8332] "no iron deficiency" NA                   NA                  
#>  [8335] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8338] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8341] "iron deficiency"    NA                   "no iron deficiency"
#>  [8344] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8347] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8350] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8353] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8356] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8359] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8362] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8365] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8368] "no iron deficiency" "iron deficiency"    NA                  
#>  [8371] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8374] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8377] "no iron deficiency" NA                   "no iron deficiency"
#>  [8380] "iron deficiency"    NA                   "no iron deficiency"
#>  [8383] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8386] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8389] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8392] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8395] NA                   "iron deficiency"    NA                  
#>  [8398] NA                   NA                   NA                  
#>  [8401] "no iron deficiency" NA                   "no iron deficiency"
#>  [8404] NA                   NA                   "iron deficiency"   
#>  [8407] NA                   NA                   NA                  
#>  [8410] "no iron deficiency" "no iron deficiency" NA                  
#>  [8413] NA                   "no iron deficiency" "iron deficiency"   
#>  [8416] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8419] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8422] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8425] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8428] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8431] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8434] "iron deficiency"    NA                   "iron deficiency"   
#>  [8437] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8440] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8443] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8446] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8449] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8452] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8455] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8458] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8461] "no iron deficiency" NA                   NA                  
#>  [8464] NA                   "iron deficiency"    "iron deficiency"   
#>  [8467] NA                   NA                   NA                  
#>  [8470] "iron deficiency"    NA                   NA                  
#>  [8473] "no iron deficiency" NA                   NA                  
#>  [8476] "iron deficiency"    NA                   "iron deficiency"   
#>  [8479] NA                   "no iron deficiency" NA                  
#>  [8482] "iron deficiency"    "iron deficiency"    NA                  
#>  [8485] NA                   NA                   "iron deficiency"   
#>  [8488] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8491] "no iron deficiency" "iron deficiency"    NA                  
#>  [8494] NA                   "no iron deficiency" "no iron deficiency"
#>  [8497] "no iron deficiency" NA                   NA                  
#>  [8500] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8503] "iron deficiency"    "iron deficiency"    NA                  
#>  [8506] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8509] "iron deficiency"    "iron deficiency"    NA                  
#>  [8512] NA                   NA                   NA                  
#>  [8515] "iron deficiency"    NA                   "iron deficiency"   
#>  [8518] "iron deficiency"    NA                   "iron deficiency"   
#>  [8521] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8524] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8527] "no iron deficiency" NA                   NA                  
#>  [8530] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8533] NA                   "iron deficiency"    "iron deficiency"   
#>  [8536] "iron deficiency"    NA                   NA                  
#>  [8539] NA                   NA                   NA                  
#>  [8542] NA                   NA                   NA                  
#>  [8545] NA                   NA                   "iron deficiency"   
#>  [8548] "no iron deficiency" "iron deficiency"    NA                  
#>  [8551] "iron deficiency"    "no iron deficiency" NA                  
#>  [8554] "no iron deficiency" "no iron deficiency" NA                  
#>  [8557] NA                   NA                   "no iron deficiency"
#>  [8560] NA                   "no iron deficiency" "no iron deficiency"
#>  [8563] NA                   NA                   "iron deficiency"   
#>  [8566] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8569] NA                   NA                   "iron deficiency"   
#>  [8572] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8575] "iron deficiency"    NA                   "iron deficiency"   
#>  [8578] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8581] NA                   NA                   "no iron deficiency"
#>  [8584] NA                   "no iron deficiency" "no iron deficiency"
#>  [8587] NA                   NA                   NA                  
#>  [8590] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8593] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8596] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8599] NA                   NA                   "iron deficiency"   
#>  [8602] NA                   NA                   NA                  
#>  [8605] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8608] NA                   "iron deficiency"    "no iron deficiency"
#>  [8611] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8614] "iron deficiency"    "iron deficiency"    NA                  
#>  [8617] "iron deficiency"    NA                   "no iron deficiency"
#>  [8620] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8623] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8626] NA                   NA                   NA                  
#>  [8629] NA                   "no iron deficiency" "no iron deficiency"
#>  [8632] NA                   "iron deficiency"    NA                  
#>  [8635] "iron deficiency"    NA                   "no iron deficiency"
#>  [8638] NA                   NA                   NA                  
#>  [8641] NA                   "iron deficiency"    NA                  
#>  [8644] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8647] "iron deficiency"    NA                   NA                  
#>  [8650] "iron deficiency"    NA                   NA                  
#>  [8653] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8656] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8659] NA                   "iron deficiency"    NA                  
#>  [8662] "no iron deficiency" NA                   "iron deficiency"   
#>  [8665] NA                   "iron deficiency"    "iron deficiency"   
#>  [8668] NA                   "iron deficiency"    NA                  
#>  [8671] NA                   "iron deficiency"    "no iron deficiency"
#>  [8674] NA                   "no iron deficiency" "iron deficiency"   
#>  [8677] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8680] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8683] "iron deficiency"    "no iron deficiency" NA                  
#>  [8686] "iron deficiency"    NA                   NA                  
#>  [8689] "iron deficiency"    NA                   "no iron deficiency"
#>  [8692] "iron deficiency"    "iron deficiency"    NA                  
#>  [8695] "no iron deficiency" "no iron deficiency" NA                  
#>  [8698] NA                   "no iron deficiency" NA                  
#>  [8701] "no iron deficiency" "no iron deficiency" NA                  
#>  [8704] NA                   NA                   NA                  
#>  [8707] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8710] "no iron deficiency" NA                   "no iron deficiency"
#>  [8713] "iron deficiency"    NA                   NA                  
#>  [8716] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8719] "iron deficiency"    "no iron deficiency" NA                  
#>  [8722] "no iron deficiency" NA                   "no iron deficiency"
#>  [8725] NA                   "iron deficiency"    NA                  
#>  [8728] NA                   "iron deficiency"    "iron deficiency"   
#>  [8731] NA                   NA                   "no iron deficiency"
#>  [8734] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8737] NA                   NA                   "no iron deficiency"
#>  [8740] "no iron deficiency" NA                   "no iron deficiency"
#>  [8743] NA                   "iron deficiency"    "iron deficiency"   
#>  [8746] NA                   "iron deficiency"    NA                  
#>  [8749] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8752] "no iron deficiency" NA                   "no iron deficiency"
#>  [8755] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8758] NA                   "iron deficiency"    "iron deficiency"   
#>  [8761] "no iron deficiency" "iron deficiency"    NA                  
#>  [8764] NA                   NA                   NA                  
#>  [8767] NA                   NA                   "iron deficiency"   
#>  [8770] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8773] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8776] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8779] "no iron deficiency" NA                   "iron deficiency"   
#>  [8782] "iron deficiency"    NA                   "iron deficiency"   
#>  [8785] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8788] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8791] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8794] NA                   "no iron deficiency" NA                  
#>  [8797] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8800] "iron deficiency"    NA                   "iron deficiency"   
#>  [8803] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8806] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8809] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8812] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8815] "iron deficiency"    NA                   NA                  
#>  [8818] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8821] "iron deficiency"    NA                   "iron deficiency"   
#>  [8824] "iron deficiency"    "iron deficiency"    NA                  
#>  [8827] "no iron deficiency" "no iron deficiency" NA                  
#>  [8830] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [8833] NA                   NA                   "iron deficiency"   
#>  [8836] "iron deficiency"    "iron deficiency"    NA                  
#>  [8839] "iron deficiency"    "no iron deficiency" NA                  
#>  [8842] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [8845] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8848] "no iron deficiency" NA                   "iron deficiency"   
#>  [8851] NA                   "iron deficiency"    "no iron deficiency"
#>  [8854] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8857] "iron deficiency"    NA                   "no iron deficiency"
#>  [8860] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8863] "iron deficiency"    "iron deficiency"    NA                  
#>  [8866] "iron deficiency"    "iron deficiency"    NA                  
#>  [8869] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8872] NA                   NA                   NA                  
#>  [8875] NA                   "no iron deficiency" "no iron deficiency"
#>  [8878] "no iron deficiency" "no iron deficiency" NA                  
#>  [8881] "iron deficiency"    NA                   "iron deficiency"   
#>  [8884] "iron deficiency"    NA                   "iron deficiency"   
#>  [8887] "iron deficiency"    NA                   "iron deficiency"   
#>  [8890] NA                   "iron deficiency"    NA                  
#>  [8893] NA                   "no iron deficiency" "no iron deficiency"
#>  [8896] NA                   NA                   "no iron deficiency"
#>  [8899] NA                   NA                   NA                  
#>  [8902] "no iron deficiency" "iron deficiency"    NA                  
#>  [8905] "iron deficiency"    "iron deficiency"    NA                  
#>  [8908] "iron deficiency"    "no iron deficiency" NA                  
#>  [8911] NA                   "no iron deficiency" NA                  
#>  [8914] NA                   NA                   "no iron deficiency"
#>  [8917] "iron deficiency"    "no iron deficiency" NA                  
#>  [8920] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [8923] "no iron deficiency" NA                   "iron deficiency"   
#>  [8926] NA                   NA                   NA                  
#>  [8929] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [8932] "no iron deficiency" NA                   "no iron deficiency"
#>  [8935] NA                   "iron deficiency"    "no iron deficiency"
#>  [8938] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8941] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [8944] "iron deficiency"    NA                   NA                  
#>  [8947] "no iron deficiency" NA                   NA                  
#>  [8950] "iron deficiency"    "iron deficiency"    NA                  
#>  [8953] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [8956] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [8959] "no iron deficiency" NA                   "no iron deficiency"
#>  [8962] "no iron deficiency" NA                   "iron deficiency"   
#>  [8965] NA                   NA                   "iron deficiency"   
#>  [8968] "no iron deficiency" NA                   "no iron deficiency"
#>  [8971] NA                   NA                   NA                  
#>  [8974] "no iron deficiency" NA                   "iron deficiency"   
#>  [8977] "iron deficiency"    NA                   "iron deficiency"   
#>  [8980] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [8983] "iron deficiency"    "iron deficiency"    NA                  
#>  [8986] NA                   "iron deficiency"    "iron deficiency"   
#>  [8989] "iron deficiency"    NA                   "iron deficiency"   
#>  [8992] NA                   "iron deficiency"    "no iron deficiency"
#>  [8995] "iron deficiency"    NA                   NA                  
#>  [8998] NA                   "iron deficiency"    NA                  
#>  [9001] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9004] NA                   "iron deficiency"    "iron deficiency"   
#>  [9007] NA                   NA                   NA                  
#>  [9010] NA                   "no iron deficiency" "iron deficiency"   
#>  [9013] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9016] "no iron deficiency" NA                   "no iron deficiency"
#>  [9019] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9022] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9025] "iron deficiency"    NA                   "no iron deficiency"
#>  [9028] NA                   "no iron deficiency" "iron deficiency"   
#>  [9031] "iron deficiency"    NA                   NA                  
#>  [9034] "iron deficiency"    "iron deficiency"    NA                  
#>  [9037] "no iron deficiency" NA                   "no iron deficiency"
#>  [9040] "no iron deficiency" NA                   "no iron deficiency"
#>  [9043] "no iron deficiency" NA                   NA                  
#>  [9046] NA                   "no iron deficiency" "iron deficiency"   
#>  [9049] "iron deficiency"    NA                   "no iron deficiency"
#>  [9052] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9055] NA                   NA                   NA                  
#>  [9058] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9061] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9064] "iron deficiency"    NA                   NA                  
#>  [9067] "iron deficiency"    "no iron deficiency" NA                  
#>  [9070] "iron deficiency"    NA                   NA                  
#>  [9073] "iron deficiency"    "iron deficiency"    NA                  
#>  [9076] NA                   NA                   "iron deficiency"   
#>  [9079] NA                   "no iron deficiency" "no iron deficiency"
#>  [9082] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9085] NA                   "no iron deficiency" "iron deficiency"   
#>  [9088] NA                   "no iron deficiency" NA                  
#>  [9091] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9094] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9097] "no iron deficiency" "iron deficiency"    NA                  
#>  [9100] "no iron deficiency" "iron deficiency"    NA                  
#>  [9103] NA                   NA                   "no iron deficiency"
#>  [9106] "iron deficiency"    "iron deficiency"    NA                  
#>  [9109] "iron deficiency"    "no iron deficiency" NA                  
#>  [9112] NA                   NA                   "iron deficiency"   
#>  [9115] "no iron deficiency" NA                   "iron deficiency"   
#>  [9118] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9121] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9124] "iron deficiency"    "iron deficiency"    NA                  
#>  [9127] NA                   NA                   "no iron deficiency"
#>  [9130] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9133] "iron deficiency"    NA                   NA                  
#>  [9136] "iron deficiency"    NA                   "no iron deficiency"
#>  [9139] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9142] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9145] NA                   "iron deficiency"    "no iron deficiency"
#>  [9148] "iron deficiency"    NA                   "iron deficiency"   
#>  [9151] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9154] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9157] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9160] "no iron deficiency" "no iron deficiency" NA                  
#>  [9163] NA                   "iron deficiency"    "iron deficiency"   
#>  [9166] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9169] "iron deficiency"    "no iron deficiency" NA                  
#>  [9172] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9175] "iron deficiency"    "iron deficiency"    NA                  
#>  [9178] NA                   "iron deficiency"    "iron deficiency"   
#>  [9181] NA                   "iron deficiency"    "no iron deficiency"
#>  [9184] "no iron deficiency" "iron deficiency"    NA                  
#>  [9187] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9190] NA                   NA                   NA                  
#>  [9193] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9196] "no iron deficiency" NA                   NA                  
#>  [9199] "no iron deficiency" NA                   "iron deficiency"   
#>  [9202] "iron deficiency"    NA                   NA                  
#>  [9205] NA                   "iron deficiency"    "iron deficiency"   
#>  [9208] NA                   "iron deficiency"    "iron deficiency"   
#>  [9211] "iron deficiency"    NA                   "iron deficiency"   
#>  [9214] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9217] "no iron deficiency" NA                   "iron deficiency"   
#>  [9220] NA                   "no iron deficiency" "iron deficiency"   
#>  [9223] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9226] "no iron deficiency" NA                   NA                  
#>  [9229] "no iron deficiency" NA                   "iron deficiency"   
#>  [9232] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9235] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9238] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9241] NA                   "iron deficiency"    "iron deficiency"   
#>  [9244] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9247] NA                   "no iron deficiency" "no iron deficiency"
#>  [9250] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9253] NA                   NA                   NA                  
#>  [9256] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9259] "no iron deficiency" NA                   "no iron deficiency"
#>  [9262] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9265] NA                   NA                   NA                  
#>  [9268] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9271] NA                   "no iron deficiency" "no iron deficiency"
#>  [9274] NA                   "no iron deficiency" "no iron deficiency"
#>  [9277] NA                   "no iron deficiency" "iron deficiency"   
#>  [9280] "no iron deficiency" "iron deficiency"    NA                  
#>  [9283] NA                   "iron deficiency"    "no iron deficiency"
#>  [9286] NA                   "no iron deficiency" NA                  
#>  [9289] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9292] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9295] NA                   "no iron deficiency" NA                  
#>  [9298] "no iron deficiency" NA                   NA                  
#>  [9301] NA                   "no iron deficiency" "iron deficiency"   
#>  [9304] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9307] NA                   "no iron deficiency" NA                  
#>  [9310] "no iron deficiency" NA                   NA                  
#>  [9313] "no iron deficiency" "no iron deficiency" NA                  
#>  [9316] NA                   "no iron deficiency" "iron deficiency"   
#>  [9319] "no iron deficiency" NA                   "no iron deficiency"
#>  [9322] "no iron deficiency" NA                   "iron deficiency"   
#>  [9325] "no iron deficiency" NA                   NA                  
#>  [9328] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9331] "iron deficiency"    "iron deficiency"    NA                  
#>  [9334] "no iron deficiency" "iron deficiency"    NA                  
#>  [9337] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9340] NA                   "iron deficiency"    NA                  
#>  [9343] "no iron deficiency" NA                   "iron deficiency"   
#>  [9346] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9349] NA                   "no iron deficiency" NA                  
#>  [9352] "iron deficiency"    NA                   "iron deficiency"   
#>  [9355] "iron deficiency"    "iron deficiency"    NA                  
#>  [9358] "no iron deficiency" NA                   NA                  
#>  [9361] "iron deficiency"    NA                   NA                  
#>  [9364] NA                   "no iron deficiency" "iron deficiency"   
#>  [9367] "no iron deficiency" NA                   NA                  
#>  [9370] NA                   "iron deficiency"    NA                  
#>  [9373] NA                   "no iron deficiency" NA                  
#>  [9376] NA                   NA                   "no iron deficiency"
#>  [9379] "no iron deficiency" NA                   "no iron deficiency"
#>  [9382] NA                   NA                   NA                  
#>  [9385] "iron deficiency"    NA                   NA                  
#>  [9388] NA                   NA                   NA                  
#>  [9391] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9394] NA                   "iron deficiency"    "iron deficiency"   
#>  [9397] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9400] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9403] "no iron deficiency" "no iron deficiency" NA                  
#>  [9406] NA                   "iron deficiency"    NA                  
#>  [9409] NA                   "iron deficiency"    NA                  
#>  [9412] NA                   "iron deficiency"    "iron deficiency"   
#>  [9415] NA                   "iron deficiency"    "iron deficiency"   
#>  [9418] "iron deficiency"    NA                   NA                  
#>  [9421] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9424] NA                   "iron deficiency"    NA                  
#>  [9427] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9430] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9433] NA                   "no iron deficiency" NA                  
#>  [9436] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9439] NA                   NA                   NA                  
#>  [9442] "no iron deficiency" NA                   "no iron deficiency"
#>  [9445] "iron deficiency"    "iron deficiency"    NA                  
#>  [9448] "no iron deficiency" NA                   NA                  
#>  [9451] NA                   NA                   NA                  
#>  [9454] "no iron deficiency" NA                   "no iron deficiency"
#>  [9457] "iron deficiency"    "no iron deficiency" NA                  
#>  [9460] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9463] "iron deficiency"    NA                   "no iron deficiency"
#>  [9466] "iron deficiency"    NA                   "iron deficiency"   
#>  [9469] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9472] "iron deficiency"    "no iron deficiency" NA                  
#>  [9475] "iron deficiency"    NA                   NA                  
#>  [9478] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9481] NA                   "iron deficiency"    NA                  
#>  [9484] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9487] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9490] "iron deficiency"    NA                   "iron deficiency"   
#>  [9493] "iron deficiency"    NA                   "iron deficiency"   
#>  [9496] NA                   "no iron deficiency" "no iron deficiency"
#>  [9499] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9502] "iron deficiency"    NA                   "iron deficiency"   
#>  [9505] NA                   NA                   "no iron deficiency"
#>  [9508] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9511] "iron deficiency"    "iron deficiency"    NA                  
#>  [9514] "no iron deficiency" NA                   "iron deficiency"   
#>  [9517] NA                   NA                   NA                  
#>  [9520] NA                   NA                   NA                  
#>  [9523] "iron deficiency"    NA                   "iron deficiency"   
#>  [9526] NA                   NA                   NA                  
#>  [9529] NA                   NA                   "iron deficiency"   
#>  [9532] NA                   "iron deficiency"    NA                  
#>  [9535] "iron deficiency"    NA                   "iron deficiency"   
#>  [9538] NA                   NA                   NA                  
#>  [9541] NA                   NA                   NA                  
#>  [9544] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9547] "no iron deficiency" NA                   "iron deficiency"   
#>  [9550] NA                   NA                   "no iron deficiency"
#>  [9553] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9556] "iron deficiency"    "iron deficiency"    NA                  
#>  [9559] "iron deficiency"    NA                   "no iron deficiency"
#>  [9562] NA                   "no iron deficiency" NA                  
#>  [9565] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9568] NA                   "iron deficiency"    NA                  
#>  [9571] NA                   "iron deficiency"    "iron deficiency"   
#>  [9574] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9577] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9580] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9583] "no iron deficiency" "no iron deficiency" NA                  
#>  [9586] "iron deficiency"    NA                   "no iron deficiency"
#>  [9589] "no iron deficiency" "iron deficiency"    NA                  
#>  [9592] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9595] "iron deficiency"    NA                   "no iron deficiency"
#>  [9598] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9601] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9604] "iron deficiency"    NA                   "no iron deficiency"
#>  [9607] NA                   "iron deficiency"    "iron deficiency"   
#>  [9610] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9613] NA                   "iron deficiency"    NA                  
#>  [9616] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9619] NA                   NA                   "no iron deficiency"
#>  [9622] "no iron deficiency" "iron deficiency"    NA                  
#>  [9625] "no iron deficiency" "no iron deficiency" NA                  
#>  [9628] "iron deficiency"    NA                   NA                  
#>  [9631] NA                   "no iron deficiency" "no iron deficiency"
#>  [9634] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9637] "no iron deficiency" NA                   "iron deficiency"   
#>  [9640] NA                   NA                   "no iron deficiency"
#>  [9643] NA                   "no iron deficiency" NA                  
#>  [9646] "no iron deficiency" NA                   NA                  
#>  [9649] NA                   "no iron deficiency" "iron deficiency"   
#>  [9652] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9655] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9658] "iron deficiency"    "iron deficiency"    NA                  
#>  [9661] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9664] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9667] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9670] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9673] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9676] "no iron deficiency" "iron deficiency"    NA                  
#>  [9679] "no iron deficiency" NA                   "iron deficiency"   
#>  [9682] "no iron deficiency" NA                   "iron deficiency"   
#>  [9685] NA                   NA                   "no iron deficiency"
#>  [9688] NA                   "no iron deficiency" NA                  
#>  [9691] NA                   "iron deficiency"    "iron deficiency"   
#>  [9694] "iron deficiency"    "no iron deficiency" NA                  
#>  [9697] NA                   NA                   NA                  
#>  [9700] NA                   "iron deficiency"    "iron deficiency"   
#>  [9703] NA                   NA                   "iron deficiency"   
#>  [9706] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9709] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9712] NA                   "no iron deficiency" NA                  
#>  [9715] "iron deficiency"    NA                   "no iron deficiency"
#>  [9718] "iron deficiency"    NA                   "no iron deficiency"
#>  [9721] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9724] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9727] NA                   "iron deficiency"    "no iron deficiency"
#>  [9730] NA                   NA                   NA                  
#>  [9733] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9736] NA                   NA                   NA                  
#>  [9739] NA                   NA                   "iron deficiency"   
#>  [9742] "no iron deficiency" "no iron deficiency" NA                  
#>  [9745] "iron deficiency"    "iron deficiency"    NA                  
#>  [9748] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#>  [9751] "iron deficiency"    "no iron deficiency" NA                  
#>  [9754] "iron deficiency"    "no iron deficiency" NA                  
#>  [9757] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9760] "no iron deficiency" NA                   "no iron deficiency"
#>  [9763] NA                   "iron deficiency"    "no iron deficiency"
#>  [9766] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9769] "no iron deficiency" NA                   NA                  
#>  [9772] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9775] "iron deficiency"    NA                   "iron deficiency"   
#>  [9778] NA                   "no iron deficiency" "no iron deficiency"
#>  [9781] NA                   "iron deficiency"    NA                  
#>  [9784] "no iron deficiency" NA                   "no iron deficiency"
#>  [9787] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9790] "iron deficiency"    "no iron deficiency" NA                  
#>  [9793] "no iron deficiency" NA                   "no iron deficiency"
#>  [9796] "no iron deficiency" NA                   "no iron deficiency"
#>  [9799] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9802] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9805] "no iron deficiency" "no iron deficiency" NA                  
#>  [9808] "no iron deficiency" "iron deficiency"    NA                  
#>  [9811] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9814] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9817] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9820] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9823] "iron deficiency"    NA                   "iron deficiency"   
#>  [9826] NA                   "iron deficiency"    "no iron deficiency"
#>  [9829] "iron deficiency"    "iron deficiency"    NA                  
#>  [9832] NA                   "no iron deficiency" NA                  
#>  [9835] "iron deficiency"    NA                   "no iron deficiency"
#>  [9838] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9841] "no iron deficiency" "iron deficiency"    NA                  
#>  [9844] "iron deficiency"    "iron deficiency"    NA                  
#>  [9847] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#>  [9850] "iron deficiency"    "no iron deficiency" NA                  
#>  [9853] "no iron deficiency" "iron deficiency"    NA                  
#>  [9856] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9859] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9862] "no iron deficiency" "no iron deficiency" NA                  
#>  [9865] NA                   "iron deficiency"    NA                  
#>  [9868] NA                   "no iron deficiency" "iron deficiency"   
#>  [9871] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9874] NA                   "iron deficiency"    "no iron deficiency"
#>  [9877] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#>  [9880] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#>  [9883] "no iron deficiency" "no iron deficiency" NA                  
#>  [9886] NA                   "iron deficiency"    NA                  
#>  [9889] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9892] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9895] NA                   "no iron deficiency" "no iron deficiency"
#>  [9898] "no iron deficiency" "no iron deficiency" NA                  
#>  [9901] NA                   "no iron deficiency" "no iron deficiency"
#>  [9904] "no iron deficiency" "iron deficiency"    NA                  
#>  [9907] NA                   "iron deficiency"    "no iron deficiency"
#>  [9910] NA                   "iron deficiency"    "no iron deficiency"
#>  [9913] NA                   "no iron deficiency" "iron deficiency"   
#>  [9916] NA                   NA                   "no iron deficiency"
#>  [9919] NA                   "iron deficiency"    NA                  
#>  [9922] NA                   "iron deficiency"    "iron deficiency"   
#>  [9925] "no iron deficiency" NA                   NA                  
#>  [9928] NA                   NA                   "iron deficiency"   
#>  [9931] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9934] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#>  [9937] NA                   "no iron deficiency" NA                  
#>  [9940] "no iron deficiency" NA                   NA                  
#>  [9943] "no iron deficiency" NA                   "iron deficiency"   
#>  [9946] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#>  [9949] "no iron deficiency" NA                   NA                  
#>  [9952] "iron deficiency"    "iron deficiency"    NA                  
#>  [9955] "iron deficiency"    NA                   "no iron deficiency"
#>  [9958] "iron deficiency"    "iron deficiency"    NA                  
#>  [9961] "iron deficiency"    NA                   NA                  
#>  [9964] "iron deficiency"    NA                   "no iron deficiency"
#>  [9967] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#>  [9970] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#>  [9973] "iron deficiency"    NA                   NA                  
#>  [9976] "no iron deficiency" NA                   "iron deficiency"   
#>  [9979] NA                   NA                   "iron deficiency"   
#>  [9982] NA                   "iron deficiency"    "no iron deficiency"
#>  [9985] "no iron deficiency" NA                   NA                  
#>  [9988] NA                   "no iron deficiency" NA                  
#>  [9991] "iron deficiency"    "iron deficiency"    NA                  
#>  [9994] "iron deficiency"    NA                   "no iron deficiency"
#>  [9997] NA                   "no iron deficiency" "no iron deficiency"
#> [10000] NA                   "no iron deficiency" "no iron deficiency"
#> [10003] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10006] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10009] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10012] "iron deficiency"    NA                   "iron deficiency"   
#> [10015] "no iron deficiency" "no iron deficiency" NA                  
#> [10018] "no iron deficiency" NA                   "iron deficiency"   
#> [10021] NA                   NA                   "iron deficiency"   
#> [10024] NA                   NA                   "no iron deficiency"
#> [10027] "no iron deficiency" NA                   NA                  
#> [10030] "iron deficiency"    "iron deficiency"    NA                  
#> [10033] NA                   NA                   "no iron deficiency"
#> [10036] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10039] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10042] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10045] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10048] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10051] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10054] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10057] NA                   "no iron deficiency" "no iron deficiency"
#> [10060] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10063] NA                   NA                   "iron deficiency"   
#> [10066] NA                   NA                   NA                  
#> [10069] NA                   NA                   "no iron deficiency"
#> [10072] "iron deficiency"    "iron deficiency"    NA                  
#> [10075] "iron deficiency"    NA                   NA                  
#> [10078] "iron deficiency"    NA                   "iron deficiency"   
#> [10081] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10084] "iron deficiency"    "no iron deficiency" NA                  
#> [10087] NA                   NA                   "iron deficiency"   
#> [10090] NA                   "iron deficiency"    NA                  
#> [10093] NA                   "no iron deficiency" NA                  
#> [10096] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10099] NA                   "no iron deficiency" "no iron deficiency"
#> [10102] NA                   "no iron deficiency" NA                  
#> [10105] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10108] NA                   "iron deficiency"    NA                  
#> [10111] NA                   NA                   NA                  
#> [10114] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10117] NA                   NA                   "no iron deficiency"
#> [10120] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10123] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10126] "iron deficiency"    NA                   NA                  
#> [10129] "no iron deficiency" NA                   NA                  
#> [10132] NA                   NA                   NA                  
#> [10135] NA                   "no iron deficiency" NA                  
#> [10138] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10141] NA                   "no iron deficiency" NA                  
#> [10144] "no iron deficiency" "iron deficiency"    NA                  
#> [10147] "no iron deficiency" "iron deficiency"    NA                  
#> [10150] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10153] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10156] "no iron deficiency" "no iron deficiency" NA                  
#> [10159] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10162] NA                   "no iron deficiency" "no iron deficiency"
#> [10165] NA                   NA                   "no iron deficiency"
#> [10168] NA                   "no iron deficiency" NA                  
#> [10171] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10174] NA                   NA                   "no iron deficiency"
#> [10177] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10180] NA                   "no iron deficiency" "no iron deficiency"
#> [10183] "no iron deficiency" NA                   NA                  
#> [10186] NA                   "no iron deficiency" "iron deficiency"   
#> [10189] NA                   NA                   "no iron deficiency"
#> [10192] "no iron deficiency" "no iron deficiency" NA                  
#> [10195] NA                   NA                   "iron deficiency"   
#> [10198] "iron deficiency"    "iron deficiency"    NA                  
#> [10201] "iron deficiency"    NA                   "no iron deficiency"
#> [10204] NA                   NA                   NA                  
#> [10207] "iron deficiency"    "no iron deficiency" NA                  
#> [10210] "no iron deficiency" "no iron deficiency" NA                  
#> [10213] NA                   NA                   "iron deficiency"   
#> [10216] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10219] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10222] "no iron deficiency" "iron deficiency"    NA                  
#> [10225] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10228] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10231] NA                   "iron deficiency"    "no iron deficiency"
#> [10234] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10237] "no iron deficiency" "no iron deficiency" NA                  
#> [10240] "iron deficiency"    "no iron deficiency" NA                  
#> [10243] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10246] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10249] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10252] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10255] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10258] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10261] "iron deficiency"    NA                   "iron deficiency"   
#> [10264] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10267] "no iron deficiency" NA                   NA                  
#> [10270] "iron deficiency"    "iron deficiency"    NA                  
#> [10273] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10276] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10279] NA                   "no iron deficiency" NA                  
#> [10282] "no iron deficiency" "no iron deficiency" NA                  
#> [10285] "iron deficiency"    "no iron deficiency" NA                  
#> [10288] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10291] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10294] NA                   "no iron deficiency" "iron deficiency"   
#> [10297] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10300] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10303] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10306] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10309] "no iron deficiency" "iron deficiency"    NA                  
#> [10312] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10315] NA                   "no iron deficiency" "no iron deficiency"
#> [10318] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10321] "iron deficiency"    "no iron deficiency" NA                  
#> [10324] NA                   NA                   "no iron deficiency"
#> [10327] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10330] "no iron deficiency" "no iron deficiency" NA                  
#> [10333] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10336] "no iron deficiency" NA                   "no iron deficiency"
#> [10339] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10342] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10345] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10348] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10351] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10354] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10357] NA                   NA                   "no iron deficiency"
#> [10360] "no iron deficiency" "no iron deficiency" NA                  
#> [10363] "iron deficiency"    NA                   "no iron deficiency"
#> [10366] NA                   "no iron deficiency" "no iron deficiency"
#> [10369] NA                   "no iron deficiency" NA                  
#> [10372] "iron deficiency"    "no iron deficiency" NA                  
#> [10375] "no iron deficiency" "no iron deficiency" NA                  
#> [10378] "iron deficiency"    "iron deficiency"    NA                  
#> [10381] NA                   "no iron deficiency" "iron deficiency"   
#> [10384] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10387] "no iron deficiency" NA                   "iron deficiency"   
#> [10390] NA                   "no iron deficiency" "iron deficiency"   
#> [10393] "iron deficiency"    NA                   NA                  
#> [10396] NA                   NA                   "no iron deficiency"
#> [10399] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10402] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10405] NA                   "no iron deficiency" NA                  
#> [10408] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10411] "no iron deficiency" "no iron deficiency" NA                  
#> [10414] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10417] "no iron deficiency" NA                   NA                  
#> [10420] NA                   NA                   NA                  
#> [10423] NA                   "no iron deficiency" NA                  
#> [10426] "no iron deficiency" NA                   "iron deficiency"   
#> [10429] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10432] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10435] NA                   NA                   "iron deficiency"   
#> [10438] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10441] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10444] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10447] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10450] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10453] NA                   NA                   "iron deficiency"   
#> [10456] NA                   "iron deficiency"    "no iron deficiency"
#> [10459] NA                   "no iron deficiency" "no iron deficiency"
#> [10462] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10465] NA                   NA                   "no iron deficiency"
#> [10468] "iron deficiency"    "iron deficiency"    NA                  
#> [10471] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10474] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10477] "iron deficiency"    "iron deficiency"    NA                  
#> [10480] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10483] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10486] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10489] "iron deficiency"    NA                   "no iron deficiency"
#> [10492] "iron deficiency"    NA                   NA                  
#> [10495] "iron deficiency"    NA                   "no iron deficiency"
#> [10498] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10501] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10504] "iron deficiency"    "iron deficiency"    NA                  
#> [10507] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10510] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10513] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10516] NA                   "iron deficiency"    NA                  
#> [10519] "iron deficiency"    NA                   "iron deficiency"   
#> [10522] NA                   NA                   "no iron deficiency"
#> [10525] NA                   NA                   NA                  
#> [10528] "no iron deficiency" NA                   "iron deficiency"   
#> [10531] "no iron deficiency" "no iron deficiency" NA                  
#> [10534] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10537] "no iron deficiency" NA                   "no iron deficiency"
#> [10540] NA                   NA                   "iron deficiency"   
#> [10543] NA                   NA                   NA                  
#> [10546] "iron deficiency"    NA                   "no iron deficiency"
#> [10549] NA                   "no iron deficiency" NA                  
#> [10552] "no iron deficiency" "no iron deficiency" NA                  
#> [10555] "iron deficiency"    NA                   "no iron deficiency"
#> [10558] "no iron deficiency" NA                   NA                  
#> [10561] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10564] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10567] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10570] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10573] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10576] "no iron deficiency" NA                   "iron deficiency"   
#> [10579] "iron deficiency"    "iron deficiency"    NA                  
#> [10582] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10585] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10588] NA                   "no iron deficiency" "iron deficiency"   
#> [10591] "no iron deficiency" "no iron deficiency" NA                  
#> [10594] NA                   "no iron deficiency" "no iron deficiency"
#> [10597] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10600] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10603] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10606] "iron deficiency"    "no iron deficiency" NA                  
#> [10609] "no iron deficiency" "no iron deficiency" NA                  
#> [10612] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10615] NA                   NA                   NA                  
#> [10618] NA                   NA                   "iron deficiency"   
#> [10621] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10624] "no iron deficiency" "iron deficiency"    NA                  
#> [10627] "no iron deficiency" "no iron deficiency" NA                  
#> [10630] "iron deficiency"    NA                   "no iron deficiency"
#> [10633] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10636] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10639] NA                   NA                   "no iron deficiency"
#> [10642] NA                   NA                   NA                  
#> [10645] NA                   "no iron deficiency" NA                  
#> [10648] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10651] "no iron deficiency" "no iron deficiency" NA                  
#> [10654] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10657] NA                   NA                   NA                  
#> [10660] NA                   "no iron deficiency" "no iron deficiency"
#> [10663] "no iron deficiency" NA                   "no iron deficiency"
#> [10666] "iron deficiency"    "no iron deficiency" NA                  
#> [10669] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10672] "no iron deficiency" NA                   "no iron deficiency"
#> [10675] "no iron deficiency" NA                   "no iron deficiency"
#> [10678] NA                   NA                   NA                  
#> [10681] "no iron deficiency" "no iron deficiency" NA                  
#> [10684] NA                   NA                   "no iron deficiency"
#> [10687] NA                   "iron deficiency"    "no iron deficiency"
#> [10690] NA                   NA                   NA                  
#> [10693] NA                   "no iron deficiency" NA                  
#> [10696] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10699] "no iron deficiency" NA                   NA                  
#> [10702] NA                   "iron deficiency"    NA                  
#> [10705] NA                   NA                   "no iron deficiency"
#> [10708] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10711] NA                   "iron deficiency"    "iron deficiency"   
#> [10714] "iron deficiency"    "no iron deficiency" NA                  
#> [10717] NA                   "no iron deficiency" "no iron deficiency"
#> [10720] NA                   "no iron deficiency" NA                  
#> [10723] NA                   NA                   NA                  
#> [10726] NA                   "iron deficiency"    NA                  
#> [10729] NA                   "iron deficiency"    "no iron deficiency"
#> [10732] "no iron deficiency" NA                   NA                  
#> [10735] "no iron deficiency" NA                   NA                  
#> [10738] "no iron deficiency" "no iron deficiency" NA                  
#> [10741] "no iron deficiency" "iron deficiency"    NA                  
#> [10744] "no iron deficiency" NA                   NA                  
#> [10747] NA                   NA                   NA                  
#> [10750] NA                   NA                   "no iron deficiency"
#> [10753] NA                   "iron deficiency"    NA                  
#> [10756] "iron deficiency"    "no iron deficiency" NA                  
#> [10759] "no iron deficiency" NA                   NA                  
#> [10762] NA                   NA                   NA                  
#> [10765] NA                   NA                   NA                  
#> [10768] "no iron deficiency" "iron deficiency"    NA                  
#> [10771] "iron deficiency"    NA                   NA                  
#> [10774] NA                   "iron deficiency"    "no iron deficiency"
#> [10777] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10780] NA                   NA                   "no iron deficiency"
#> [10783] NA                   NA                   "no iron deficiency"
#> [10786] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10789] NA                   NA                   NA                  
#> [10792] NA                   NA                   NA                  
#> [10795] NA                   NA                   "no iron deficiency"
#> [10798] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10801] NA                   "no iron deficiency" "no iron deficiency"
#> [10804] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10807] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10810] NA                   "no iron deficiency" "no iron deficiency"
#> [10813] NA                   "no iron deficiency" NA                  
#> [10816] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10819] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10822] "iron deficiency"    NA                   NA                  
#> [10825] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10828] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10831] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10834] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10837] "no iron deficiency" "no iron deficiency" NA                  
#> [10840] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [10843] "no iron deficiency" NA                   NA                  
#> [10846] "iron deficiency"    NA                   "no iron deficiency"
#> [10849] "no iron deficiency" NA                   "no iron deficiency"
#> [10852] "no iron deficiency" "no iron deficiency" NA                  
#> [10855] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10858] NA                   "no iron deficiency" NA                  
#> [10861] NA                   "no iron deficiency" NA                  
#> [10864] "no iron deficiency" "no iron deficiency" NA                  
#> [10867] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10870] "no iron deficiency" NA                   "no iron deficiency"
#> [10873] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10876] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10879] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10882] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10885] NA                   "iron deficiency"    "iron deficiency"   
#> [10888] NA                   "iron deficiency"    "iron deficiency"   
#> [10891] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10894] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10897] NA                   "iron deficiency"    "no iron deficiency"
#> [10900] NA                   "no iron deficiency" NA                  
#> [10903] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10906] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [10909] "iron deficiency"    "iron deficiency"    NA                  
#> [10912] NA                   "iron deficiency"    "no iron deficiency"
#> [10915] "iron deficiency"    "iron deficiency"    NA                  
#> [10918] "no iron deficiency" NA                   "iron deficiency"   
#> [10921] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10924] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10927] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [10930] "no iron deficiency" "iron deficiency"    NA                  
#> [10933] "no iron deficiency" NA                   "iron deficiency"   
#> [10936] "no iron deficiency" NA                   "iron deficiency"   
#> [10939] "no iron deficiency" NA                   "no iron deficiency"
#> [10942] "no iron deficiency" "no iron deficiency" NA                  
#> [10945] NA                   "no iron deficiency" NA                  
#> [10948] "no iron deficiency" NA                   NA                  
#> [10951] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10954] "iron deficiency"    NA                   NA                  
#> [10957] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [10960] "no iron deficiency" "no iron deficiency" NA                  
#> [10963] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [10966] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10969] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [10972] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10975] "iron deficiency"    "no iron deficiency" NA                  
#> [10978] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [10981] "no iron deficiency" NA                   "no iron deficiency"
#> [10984] NA                   NA                   "no iron deficiency"
#> [10987] NA                   "iron deficiency"    "iron deficiency"   
#> [10990] NA                   "no iron deficiency" "no iron deficiency"
#> [10993] "no iron deficiency" NA                   "no iron deficiency"
#> [10996] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [10999] NA                   NA                   "no iron deficiency"
#> [11002] NA                   "no iron deficiency" "iron deficiency"   
#> [11005] NA                   "iron deficiency"    "iron deficiency"   
#> [11008] "no iron deficiency" NA                   "no iron deficiency"
#> [11011] "iron deficiency"    "iron deficiency"    NA                  
#> [11014] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11017] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11020] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11023] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11026] "iron deficiency"    NA                   "iron deficiency"   
#> [11029] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11032] "no iron deficiency" NA                   "no iron deficiency"
#> [11035] NA                   "no iron deficiency" "iron deficiency"   
#> [11038] "no iron deficiency" NA                   NA                  
#> [11041] NA                   "no iron deficiency" "no iron deficiency"
#> [11044] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11047] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11050] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11053] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11056] "no iron deficiency" "iron deficiency"    NA                  
#> [11059] NA                   "iron deficiency"    "no iron deficiency"
#> [11062] NA                   NA                   "iron deficiency"   
#> [11065] "no iron deficiency" "iron deficiency"    NA                  
#> [11068] NA                   NA                   NA                  
#> [11071] "no iron deficiency" "no iron deficiency" NA                  
#> [11074] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11077] "no iron deficiency" NA                   NA                  
#> [11080] "iron deficiency"    "no iron deficiency" NA                  
#> [11083] "no iron deficiency" "no iron deficiency" NA                  
#> [11086] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11089] NA                   NA                   "no iron deficiency"
#> [11092] NA                   NA                   NA                  
#> [11095] "no iron deficiency" "no iron deficiency" NA                  
#> [11098] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11101] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11104] "iron deficiency"    NA                   "iron deficiency"   
#> [11107] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11110] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11113] "iron deficiency"    "no iron deficiency" NA                  
#> [11116] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11119] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11122] NA                   NA                   NA                  
#> [11125] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11128] "no iron deficiency" NA                   "iron deficiency"   
#> [11131] NA                   NA                   "no iron deficiency"
#> [11134] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11137] NA                   "no iron deficiency" "iron deficiency"   
#> [11140] "iron deficiency"    NA                   NA                  
#> [11143] "iron deficiency"    NA                   "no iron deficiency"
#> [11146] "iron deficiency"    "iron deficiency"    NA                  
#> [11149] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11152] "no iron deficiency" NA                   NA                  
#> [11155] NA                   "iron deficiency"    "iron deficiency"   
#> [11158] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11161] "iron deficiency"    "iron deficiency"    NA                  
#> [11164] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11167] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11170] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11173] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11176] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11179] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11182] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11185] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11188] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11191] NA                   "iron deficiency"    NA                  
#> [11194] "iron deficiency"    "no iron deficiency" NA                  
#> [11197] "iron deficiency"    "iron deficiency"    NA                  
#> [11200] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11203] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11206] "iron deficiency"    NA                   "iron deficiency"   
#> [11209] "no iron deficiency" "no iron deficiency" NA                  
#> [11212] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11215] NA                   "no iron deficiency" "no iron deficiency"
#> [11218] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11221] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11224] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11227] "no iron deficiency" "no iron deficiency" NA                  
#> [11230] NA                   NA                   "no iron deficiency"
#> [11233] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11236] "iron deficiency"    "iron deficiency"    NA                  
#> [11239] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11242] NA                   "no iron deficiency" "iron deficiency"   
#> [11245] NA                   NA                   "no iron deficiency"
#> [11248] NA                   "iron deficiency"    "no iron deficiency"
#> [11251] "no iron deficiency" "no iron deficiency" NA                  
#> [11254] "no iron deficiency" NA                   "no iron deficiency"
#> [11257] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11260] "no iron deficiency" "no iron deficiency" NA                  
#> [11263] "no iron deficiency" "no iron deficiency" NA                  
#> [11266] "iron deficiency"    NA                   "iron deficiency"   
#> [11269] "iron deficiency"    NA                   "no iron deficiency"
#> [11272] "no iron deficiency" NA                   "no iron deficiency"
#> [11275] "no iron deficiency" "no iron deficiency" NA                  
#> [11278] NA                   NA                   NA                  
#> [11281] NA                   "no iron deficiency" "no iron deficiency"
#> [11284] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11287] NA                   NA                   NA                  
#> [11290] NA                   NA                   "iron deficiency"   
#> [11293] NA                   NA                   "iron deficiency"   
#> [11296] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11299] NA                   "no iron deficiency" NA                  
#> [11302] "no iron deficiency" "iron deficiency"    NA                  
#> [11305] "iron deficiency"    "iron deficiency"    NA                  
#> [11308] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11311] NA                   NA                   NA                  
#> [11314] "no iron deficiency" NA                   "no iron deficiency"
#> [11317] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11320] "iron deficiency"    NA                   NA                  
#> [11323] "no iron deficiency" NA                   "no iron deficiency"
#> [11326] "no iron deficiency" "no iron deficiency" NA                  
#> [11329] NA                   "no iron deficiency" NA                  
#> [11332] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11335] "no iron deficiency" NA                   "no iron deficiency"
#> [11338] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11341] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11344] "no iron deficiency" NA                   NA                  
#> [11347] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11350] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11353] "no iron deficiency" "iron deficiency"    NA                  
#> [11356] "no iron deficiency" NA                   "no iron deficiency"
#> [11359] "no iron deficiency" "no iron deficiency" NA                  
#> [11362] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11365] "no iron deficiency" "iron deficiency"    NA                  
#> [11368] NA                   "no iron deficiency" NA                  
#> [11371] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11374] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11377] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11380] "no iron deficiency" "no iron deficiency" NA                  
#> [11383] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11386] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11389] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11392] NA                   "no iron deficiency" "no iron deficiency"
#> [11395] "no iron deficiency" NA                   "iron deficiency"   
#> [11398] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11401] NA                   NA                   NA                  
#> [11404] "no iron deficiency" NA                   "no iron deficiency"
#> [11407] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11410] NA                   "no iron deficiency" "iron deficiency"   
#> [11413] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11416] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11419] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11422] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11425] NA                   "iron deficiency"    "iron deficiency"   
#> [11428] "iron deficiency"    NA                   NA                  
#> [11431] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11434] NA                   NA                   NA                  
#> [11437] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11440] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11443] NA                   "no iron deficiency" "iron deficiency"   
#> [11446] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11449] NA                   "no iron deficiency" "no iron deficiency"
#> [11452] NA                   NA                   NA                  
#> [11455] NA                   NA                   NA                  
#> [11458] NA                   NA                   NA                  
#> [11461] NA                   NA                   NA                  
#> [11464] NA                   NA                   "iron deficiency"   
#> [11467] NA                   NA                   NA                  
#> [11470] NA                   NA                   NA                  
#> [11473] NA                   NA                   NA                  
#> [11476] NA                   NA                   NA                  
#> [11479] "iron deficiency"    NA                   NA                  
#> [11482] NA                   NA                   NA                  
#> [11485] NA                   NA                   NA                  
#> [11488] NA                   NA                   NA                  
#> [11491] NA                   "no iron deficiency" "iron deficiency"   
#> [11494] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11497] "no iron deficiency" "no iron deficiency" NA                  
#> [11500] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11503] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11506] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11509] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11512] "iron deficiency"    "iron deficiency"    NA                  
#> [11515] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11518] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11521] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11524] "no iron deficiency" "no iron deficiency" NA                  
#> [11527] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11530] "no iron deficiency" "iron deficiency"    NA                  
#> [11533] "no iron deficiency" "no iron deficiency" NA                  
#> [11536] NA                   "no iron deficiency" "iron deficiency"   
#> [11539] "no iron deficiency" NA                   "no iron deficiency"
#> [11542] NA                   "no iron deficiency" "no iron deficiency"
#> [11545] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11548] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11551] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11554] "iron deficiency"    NA                   "no iron deficiency"
#> [11557] "iron deficiency"    "no iron deficiency" NA                  
#> [11560] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11563] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11566] "iron deficiency"    NA                   "iron deficiency"   
#> [11569] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11572] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11575] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11578] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11581] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11584] "no iron deficiency" "iron deficiency"    NA                  
#> [11587] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11590] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11593] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11596] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11599] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11602] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11605] NA                   "no iron deficiency" "no iron deficiency"
#> [11608] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11611] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11614] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11617] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11620] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11623] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11626] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11629] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11632] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11635] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11638] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11641] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11644] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11647] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11650] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11653] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11656] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11659] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11662] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11665] "iron deficiency"    "no iron deficiency" NA                  
#> [11668] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11671] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11674] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11677] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11680] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11683] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11686] "no iron deficiency" NA                   "no iron deficiency"
#> [11689] NA                   "no iron deficiency" "iron deficiency"   
#> [11692] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11695] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11698] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11701] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11704] "iron deficiency"    NA                   "no iron deficiency"
#> [11707] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11710] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11713] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11716] "iron deficiency"    "no iron deficiency" NA                  
#> [11719] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11722] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11725] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11728] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11731] "iron deficiency"    NA                   "iron deficiency"   
#> [11734] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11737] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11740] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11743] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11746] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11749] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11752] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11755] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11758] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11761] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11764] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11767] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11770] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11773] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11776] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11779] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11782] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11785] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11788] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11791] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11794] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11797] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11800] NA                   "iron deficiency"    "iron deficiency"   
#> [11803] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11806] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11809] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11812] "iron deficiency"    "no iron deficiency" NA                  
#> [11815] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11818] "iron deficiency"    NA                   "no iron deficiency"
#> [11821] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11824] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11827] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11830] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11833] "no iron deficiency" NA                   "iron deficiency"   
#> [11836] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11839] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11842] NA                   "iron deficiency"    "no iron deficiency"
#> [11845] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11848] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11851] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [11854] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11857] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11860] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11863] "iron deficiency"    NA                   "iron deficiency"   
#> [11866] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11869] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11872] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11875] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11878] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11881] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11884] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11887] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11890] NA                   "no iron deficiency" "no iron deficiency"
#> [11893] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11896] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11899] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11902] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11905] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11908] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11911] "no iron deficiency" NA                   "iron deficiency"   
#> [11914] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11917] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11920] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11923] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11926] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11929] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [11932] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11935] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11938] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11941] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11944] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11947] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [11950] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11953] NA                   "iron deficiency"    NA                  
#> [11956] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11959] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [11962] "no iron deficiency" NA                   "no iron deficiency"
#> [11965] "no iron deficiency" "no iron deficiency" NA                  
#> [11968] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [11971] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11974] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11977] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11980] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [11983] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [11986] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [11989] NA                   "no iron deficiency" "iron deficiency"   
#> [11992] "no iron deficiency" "no iron deficiency" NA                  
#> [11995] NA                   NA                   "iron deficiency"   
#> [11998] "no iron deficiency" NA                   "no iron deficiency"
#> [12001] "iron deficiency"    NA                   "iron deficiency"   
#> [12004] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12007] NA                   "iron deficiency"    NA                  
#> [12010] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12013] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12016] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12019] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12022] "no iron deficiency" "no iron deficiency" NA                  
#> [12025] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12028] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12031] "iron deficiency"    "no iron deficiency" NA                  
#> [12034] NA                   "no iron deficiency" "iron deficiency"   
#> [12037] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12040] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12043] "iron deficiency"    "iron deficiency"    NA                  
#> [12046] "no iron deficiency" "iron deficiency"    NA                  
#> [12049] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12052] NA                   "no iron deficiency" "no iron deficiency"
#> [12055] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12058] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12061] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12064] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12067] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12070] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12073] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12076] "iron deficiency"    "iron deficiency"    NA                  
#> [12079] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12082] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12085] "iron deficiency"    NA                   "no iron deficiency"
#> [12088] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12091] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12094] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12097] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12100] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12103] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12106] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12109] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12112] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12115] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12118] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12121] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12124] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12127] NA                   NA                   "no iron deficiency"
#> [12130] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12133] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12136] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12139] "iron deficiency"    NA                   "iron deficiency"   
#> [12142] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12145] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12148] "iron deficiency"    "no iron deficiency" NA                  
#> [12151] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12154] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12157] "no iron deficiency" "no iron deficiency" NA                  
#> [12160] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12163] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12166] NA                   "no iron deficiency" "iron deficiency"   
#> [12169] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12172] "no iron deficiency" NA                   "no iron deficiency"
#> [12175] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12178] NA                   "no iron deficiency" "no iron deficiency"
#> [12181] NA                   "no iron deficiency" "no iron deficiency"
#> [12184] NA                   "no iron deficiency" "no iron deficiency"
#> [12187] "iron deficiency"    NA                   "no iron deficiency"
#> [12190] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12193] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12196] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12199] "iron deficiency"    "no iron deficiency" NA                  
#> [12202] NA                   "no iron deficiency" NA                  
#> [12205] "no iron deficiency" NA                   "iron deficiency"   
#> [12208] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12211] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12214] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12217] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12220] "iron deficiency"    "no iron deficiency" NA                  
#> [12223] NA                   "iron deficiency"    "no iron deficiency"
#> [12226] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12229] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12232] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12235] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12238] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12241] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12244] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12247] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12250] NA                   "iron deficiency"    "no iron deficiency"
#> [12253] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12256] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12259] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12262] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12265] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12268] "iron deficiency"    NA                   "iron deficiency"   
#> [12271] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12274] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12277] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12280] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12283] NA                   "no iron deficiency" "iron deficiency"   
#> [12286] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12289] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12292] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12295] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12298] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12301] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12304] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12307] NA                   "no iron deficiency" "iron deficiency"   
#> [12310] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12313] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12316] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12319] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12322] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12325] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12328] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12331] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12334] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12337] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12340] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12343] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12346] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12349] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12352] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12355] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12358] "no iron deficiency" "iron deficiency"    NA                  
#> [12361] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12364] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12367] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12370] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12373] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12376] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12379] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12382] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12385] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12388] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12391] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12394] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12397] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12400] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12403] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12406] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12409] "iron deficiency"    NA                   "no iron deficiency"
#> [12412] "iron deficiency"    NA                   "no iron deficiency"
#> [12415] "no iron deficiency" NA                   "no iron deficiency"
#> [12418] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12421] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12424] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12427] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12430] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12433] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12436] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12439] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12442] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12445] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12448] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12451] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12454] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12457] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12460] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12463] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12466] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12469] "iron deficiency"    "no iron deficiency" NA                  
#> [12472] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12475] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12478] "iron deficiency"    "iron deficiency"    NA                  
#> [12481] "iron deficiency"    NA                   "no iron deficiency"
#> [12484] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12487] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12490] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12493] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12496] NA                   NA                   "no iron deficiency"
#> [12499] NA                   "no iron deficiency" "iron deficiency"   
#> [12502] NA                   "iron deficiency"    "no iron deficiency"
#> [12505] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12508] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12511] "iron deficiency"    NA                   "iron deficiency"   
#> [12514] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12517] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12520] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12523] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12526] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12529] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12532] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12535] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12538] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12541] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12544] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12547] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12550] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12553] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12556] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12559] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12562] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12565] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12568] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12571] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12574] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12577] "no iron deficiency" "no iron deficiency" NA                  
#> [12580] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12583] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12586] "no iron deficiency" NA                   "no iron deficiency"
#> [12589] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12592] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12595] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12598] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12601] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12604] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12607] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12610] "no iron deficiency" "iron deficiency"    NA                  
#> [12613] "no iron deficiency" NA                   "iron deficiency"   
#> [12616] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12619] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12622] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12625] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12628] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12631] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12634] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12637] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12640] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12643] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12646] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12649] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12652] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12655] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12658] "no iron deficiency" "no iron deficiency" NA                  
#> [12661] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12664] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12667] NA                   "iron deficiency"    "iron deficiency"   
#> [12670] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12673] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12676] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12679] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12682] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12685] NA                   "no iron deficiency" "no iron deficiency"
#> [12688] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12691] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12694] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12697] "no iron deficiency" "iron deficiency"    NA                  
#> [12700] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12703] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12706] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12709] "no iron deficiency" "iron deficiency"    NA                  
#> [12712] "no iron deficiency" NA                   "no iron deficiency"
#> [12715] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12718] NA                   "iron deficiency"    "no iron deficiency"
#> [12721] NA                   "no iron deficiency" "iron deficiency"   
#> [12724] "iron deficiency"    "no iron deficiency" NA                  
#> [12727] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12730] NA                   NA                   NA                  
#> [12733] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12736] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12739] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12742] NA                   "iron deficiency"    "no iron deficiency"
#> [12745] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12748] "iron deficiency"    "iron deficiency"    NA                  
#> [12751] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12754] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12757] "iron deficiency"    NA                   "no iron deficiency"
#> [12760] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [12763] "iron deficiency"    "no iron deficiency" NA                  
#> [12766] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12769] "iron deficiency"    NA                   "iron deficiency"   
#> [12772] "iron deficiency"    "iron deficiency"    NA                  
#> [12775] "no iron deficiency" "iron deficiency"    NA                  
#> [12778] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12781] NA                   "iron deficiency"    "no iron deficiency"
#> [12784] NA                   "iron deficiency"    "no iron deficiency"
#> [12787] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [12790] "iron deficiency"    NA                   "no iron deficiency"
#> [12793] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12796] "iron deficiency"    "no iron deficiency" NA                  
#> [12799] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12802] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12805] NA                   "iron deficiency"    NA                  
#> [12808] NA                   NA                   "iron deficiency"   
#> [12811] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12814] NA                   "iron deficiency"    NA                  
#> [12817] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [12820] NA                   "iron deficiency"    "iron deficiency"   
#> [12823] "no iron deficiency" NA                   "iron deficiency"   
#> [12826] "no iron deficiency" NA                   "no iron deficiency"
#> [12829] "iron deficiency"    "no iron deficiency" NA                  
#> [12832] "no iron deficiency" NA                   "no iron deficiency"
#> [12835] "no iron deficiency" NA                   NA                  
#> [12838] "no iron deficiency" NA                   "iron deficiency"   
#> [12841] NA                   "no iron deficiency" "iron deficiency"   
#> [12844] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12847] "no iron deficiency" NA                   "iron deficiency"   
#> [12850] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12853] NA                   "iron deficiency"    "iron deficiency"   
#> [12856] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12859] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12862] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12865] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12868] "no iron deficiency" "no iron deficiency" NA                  
#> [12871] NA                   "iron deficiency"    "no iron deficiency"
#> [12874] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [12877] "iron deficiency"    "iron deficiency"    NA                  
#> [12880] "no iron deficiency" NA                   "iron deficiency"   
#> [12883] "iron deficiency"    NA                   "iron deficiency"   
#> [12886] NA                   NA                   "no iron deficiency"
#> [12889] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [12892] "no iron deficiency" NA                   "iron deficiency"   
#> [12895] NA                   "iron deficiency"    "no iron deficiency"
#> [12898] NA                   "no iron deficiency" "no iron deficiency"
#> [12901] "iron deficiency"    NA                   "no iron deficiency"
#> [12904] "iron deficiency"    NA                   "iron deficiency"   
#> [12907] NA                   "no iron deficiency" "iron deficiency"   
#> [12910] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12913] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12916] NA                   NA                   NA                  
#> [12919] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [12922] NA                   "no iron deficiency" NA                  
#> [12925] "no iron deficiency" NA                   "no iron deficiency"
#> [12928] NA                   NA                   "no iron deficiency"
#> [12931] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12934] NA                   NA                   NA                  
#> [12937] NA                   NA                   NA                  
#> [12940] "no iron deficiency" NA                   "no iron deficiency"
#> [12943] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12946] NA                   NA                   "no iron deficiency"
#> [12949] "iron deficiency"    "no iron deficiency" NA                  
#> [12952] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12955] NA                   NA                   NA                  
#> [12958] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12961] NA                   NA                   NA                  
#> [12964] NA                   "no iron deficiency" "no iron deficiency"
#> [12967] NA                   "no iron deficiency" NA                  
#> [12970] "iron deficiency"    NA                   NA                  
#> [12973] "iron deficiency"    NA                   "no iron deficiency"
#> [12976] NA                   NA                   "no iron deficiency"
#> [12979] "iron deficiency"    NA                   NA                  
#> [12982] "no iron deficiency" NA                   NA                  
#> [12985] NA                   "iron deficiency"    "iron deficiency"   
#> [12988] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [12991] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [12994] "no iron deficiency" NA                   "no iron deficiency"
#> [12997] NA                   "no iron deficiency" "iron deficiency"   
#> [13000] NA                   "no iron deficiency" "no iron deficiency"
#> [13003] NA                   "iron deficiency"    NA                  
#> [13006] "iron deficiency"    "no iron deficiency" NA                  
#> [13009] NA                   NA                   NA                  
#> [13012] NA                   "iron deficiency"    "iron deficiency"   
#> [13015] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13018] "no iron deficiency" NA                   "iron deficiency"   
#> [13021] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13024] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13027] "iron deficiency"    NA                   "no iron deficiency"
#> [13030] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13033] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13036] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13039] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13042] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13045] "no iron deficiency" NA                   "iron deficiency"   
#> [13048] "no iron deficiency" NA                   NA                  
#> [13051] NA                   NA                   "no iron deficiency"
#> [13054] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13057] NA                   "no iron deficiency" "no iron deficiency"
#> [13060] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13063] NA                   NA                   "iron deficiency"   
#> [13066] NA                   NA                   NA                  
#> [13069] "iron deficiency"    NA                   NA                  
#> [13072] NA                   NA                   "no iron deficiency"
#> [13075] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13078] NA                   "iron deficiency"    "no iron deficiency"
#> [13081] NA                   NA                   "no iron deficiency"
#> [13084] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13087] "no iron deficiency" "iron deficiency"    NA                  
#> [13090] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13093] "no iron deficiency" "iron deficiency"    NA                  
#> [13096] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13099] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13102] NA                   "no iron deficiency" "no iron deficiency"
#> [13105] NA                   "iron deficiency"    NA                  
#> [13108] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13111] NA                   "no iron deficiency" "iron deficiency"   
#> [13114] "no iron deficiency" "iron deficiency"    NA                  
#> [13117] NA                   "iron deficiency"    "iron deficiency"   
#> [13120] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13123] NA                   "no iron deficiency" "no iron deficiency"
#> [13126] "iron deficiency"    NA                   "no iron deficiency"
#> [13129] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13132] "no iron deficiency" NA                   "iron deficiency"   
#> [13135] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13138] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13141] "no iron deficiency" NA                   "iron deficiency"   
#> [13144] "iron deficiency"    "iron deficiency"    NA                  
#> [13147] "no iron deficiency" "iron deficiency"    NA                  
#> [13150] "iron deficiency"    "no iron deficiency" NA                  
#> [13153] "no iron deficiency" NA                   "iron deficiency"   
#> [13156] "iron deficiency"    "no iron deficiency" NA                  
#> [13159] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13162] "no iron deficiency" NA                   "iron deficiency"   
#> [13165] NA                   "no iron deficiency" "no iron deficiency"
#> [13168] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13171] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13174] NA                   "no iron deficiency" "iron deficiency"   
#> [13177] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13180] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13183] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13186] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13189] "no iron deficiency" "no iron deficiency" NA                  
#> [13192] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13195] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13198] NA                   "iron deficiency"    NA                  
#> [13201] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13204] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13207] "no iron deficiency" NA                   "no iron deficiency"
#> [13210] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13213] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13216] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13219] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13222] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13225] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13228] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13231] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13234] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13237] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13240] "no iron deficiency" NA                   NA                  
#> [13243] "no iron deficiency" NA                   "no iron deficiency"
#> [13246] NA                   NA                   NA                  
#> [13249] "iron deficiency"    NA                   "iron deficiency"   
#> [13252] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13255] "no iron deficiency" NA                   NA                  
#> [13258] "iron deficiency"    "no iron deficiency" NA                  
#> [13261] "iron deficiency"    NA                   "iron deficiency"   
#> [13264] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13267] NA                   NA                   NA                  
#> [13270] "no iron deficiency" NA                   "iron deficiency"   
#> [13273] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13276] NA                   NA                   "iron deficiency"   
#> [13279] NA                   "iron deficiency"    NA                  
#> [13282] NA                   "no iron deficiency" "no iron deficiency"
#> [13285] "no iron deficiency" NA                   NA                  
#> [13288] "no iron deficiency" NA                   "iron deficiency"   
#> [13291] "iron deficiency"    NA                   "no iron deficiency"
#> [13294] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13297] "no iron deficiency" NA                   "iron deficiency"   
#> [13300] "iron deficiency"    NA                   NA                  
#> [13303] "no iron deficiency" NA                   "no iron deficiency"
#> [13306] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13309] NA                   "iron deficiency"    NA                  
#> [13312] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13315] NA                   "no iron deficiency" "no iron deficiency"
#> [13318] "iron deficiency"    "no iron deficiency" NA                  
#> [13321] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13324] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13327] NA                   "no iron deficiency" "no iron deficiency"
#> [13330] "no iron deficiency" NA                   "no iron deficiency"
#> [13333] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13336] NA                   "iron deficiency"    "iron deficiency"   
#> [13339] "iron deficiency"    NA                   "no iron deficiency"
#> [13342] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13345] "iron deficiency"    "no iron deficiency" NA                  
#> [13348] "no iron deficiency" "no iron deficiency" NA                  
#> [13351] "no iron deficiency" NA                   "no iron deficiency"
#> [13354] "iron deficiency"    NA                   "no iron deficiency"
#> [13357] "no iron deficiency" "iron deficiency"    NA                  
#> [13360] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13363] NA                   "no iron deficiency" "iron deficiency"   
#> [13366] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13369] NA                   "iron deficiency"    "no iron deficiency"
#> [13372] "iron deficiency"    "no iron deficiency" NA                  
#> [13375] "iron deficiency"    NA                   "no iron deficiency"
#> [13378] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13381] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13384] NA                   "iron deficiency"    "no iron deficiency"
#> [13387] "no iron deficiency" NA                   NA                  
#> [13390] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13393] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13396] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13399] NA                   NA                   "no iron deficiency"
#> [13402] NA                   NA                   "no iron deficiency"
#> [13405] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13408] "iron deficiency"    "no iron deficiency" NA                  
#> [13411] NA                   "iron deficiency"    NA                  
#> [13414] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13417] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13420] "iron deficiency"    NA                   NA                  
#> [13423] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13426] "no iron deficiency" NA                   "iron deficiency"   
#> [13429] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13432] "no iron deficiency" NA                   "no iron deficiency"
#> [13435] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13438] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13441] "iron deficiency"    "no iron deficiency" NA                  
#> [13444] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13447] "iron deficiency"    "iron deficiency"    NA                  
#> [13450] "iron deficiency"    "no iron deficiency" NA                  
#> [13453] NA                   "iron deficiency"    NA                  
#> [13456] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13459] "no iron deficiency" NA                   "no iron deficiency"
#> [13462] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13465] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13468] "iron deficiency"    NA                   "iron deficiency"   
#> [13471] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13474] NA                   "iron deficiency"    "no iron deficiency"
#> [13477] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13480] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13483] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13486] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13489] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13492] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13495] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13498] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13501] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13504] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13507] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13510] "iron deficiency"    "no iron deficiency" NA                  
#> [13513] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13516] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13519] "no iron deficiency" NA                   "no iron deficiency"
#> [13522] NA                   "no iron deficiency" NA                  
#> [13525] "no iron deficiency" "no iron deficiency" NA                  
#> [13528] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13531] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13534] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13537] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13540] NA                   "no iron deficiency" "iron deficiency"   
#> [13543] "no iron deficiency" "no iron deficiency" NA                  
#> [13546] "iron deficiency"    "no iron deficiency" NA                  
#> [13549] "iron deficiency"    NA                   "no iron deficiency"
#> [13552] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13555] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13558] NA                   NA                   NA                  
#> [13561] NA                   "no iron deficiency" "no iron deficiency"
#> [13564] NA                   "no iron deficiency" "no iron deficiency"
#> [13567] "iron deficiency"    NA                   "no iron deficiency"
#> [13570] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13573] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13576] "iron deficiency"    "iron deficiency"    NA                  
#> [13579] "iron deficiency"    NA                   "iron deficiency"   
#> [13582] "no iron deficiency" "iron deficiency"    NA                  
#> [13585] NA                   "no iron deficiency" NA                  
#> [13588] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13591] "iron deficiency"    "no iron deficiency" NA                  
#> [13594] NA                   NA                   NA                  
#> [13597] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13600] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13603] "iron deficiency"    "no iron deficiency" NA                  
#> [13606] NA                   "iron deficiency"    NA                  
#> [13609] "no iron deficiency" NA                   "iron deficiency"   
#> [13612] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13615] "no iron deficiency" "iron deficiency"    NA                  
#> [13618] "no iron deficiency" NA                   "iron deficiency"   
#> [13621] "no iron deficiency" "iron deficiency"    NA                  
#> [13624] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13627] NA                   NA                   "no iron deficiency"
#> [13630] "iron deficiency"    "no iron deficiency" NA                  
#> [13633] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13636] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13639] "iron deficiency"    NA                   NA                  
#> [13642] "iron deficiency"    NA                   "no iron deficiency"
#> [13645] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13648] NA                   "iron deficiency"    "no iron deficiency"
#> [13651] "iron deficiency"    "iron deficiency"    NA                  
#> [13654] "iron deficiency"    NA                   "iron deficiency"   
#> [13657] NA                   "iron deficiency"    NA                  
#> [13660] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13663] NA                   NA                   "no iron deficiency"
#> [13666] NA                   "no iron deficiency" NA                  
#> [13669] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13672] NA                   "no iron deficiency" "no iron deficiency"
#> [13675] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13678] NA                   "no iron deficiency" "no iron deficiency"
#> [13681] NA                   NA                   NA                  
#> [13684] "iron deficiency"    NA                   NA                  
#> [13687] "iron deficiency"    NA                   NA                  
#> [13690] NA                   "iron deficiency"    NA                  
#> [13693] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13696] NA                   NA                   "no iron deficiency"
#> [13699] "iron deficiency"    "iron deficiency"    NA                  
#> [13702] "iron deficiency"    NA                   NA                  
#> [13705] "no iron deficiency" "no iron deficiency" NA                  
#> [13708] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13711] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13714] "no iron deficiency" NA                   "no iron deficiency"
#> [13717] NA                   "iron deficiency"    NA                  
#> [13720] "iron deficiency"    "iron deficiency"    NA                  
#> [13723] NA                   "iron deficiency"    "no iron deficiency"
#> [13726] NA                   NA                   "iron deficiency"   
#> [13729] NA                   "no iron deficiency" NA                  
#> [13732] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [13735] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13738] NA                   "iron deficiency"    "no iron deficiency"
#> [13741] "iron deficiency"    NA                   "no iron deficiency"
#> [13744] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13747] "no iron deficiency" NA                   "iron deficiency"   
#> [13750] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13753] NA                   NA                   "no iron deficiency"
#> [13756] "iron deficiency"    NA                   NA                  
#> [13759] "iron deficiency"    NA                   NA                  
#> [13762] NA                   "no iron deficiency" NA                  
#> [13765] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13768] "iron deficiency"    NA                   NA                  
#> [13771] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13774] "iron deficiency"    NA                   NA                  
#> [13777] NA                   NA                   NA                  
#> [13780] NA                   "iron deficiency"    NA                  
#> [13783] "iron deficiency"    NA                   "no iron deficiency"
#> [13786] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13789] "iron deficiency"    NA                   NA                  
#> [13792] "iron deficiency"    NA                   NA                  
#> [13795] "iron deficiency"    NA                   "no iron deficiency"
#> [13798] "iron deficiency"    NA                   NA                  
#> [13801] "no iron deficiency" "no iron deficiency" NA                  
#> [13804] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13807] "no iron deficiency" NA                   NA                  
#> [13810] NA                   "no iron deficiency" "iron deficiency"   
#> [13813] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13816] "iron deficiency"    "no iron deficiency" NA                  
#> [13819] "iron deficiency"    NA                   "iron deficiency"   
#> [13822] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13825] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13828] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13831] "iron deficiency"    NA                   NA                  
#> [13834] NA                   "no iron deficiency" NA                  
#> [13837] "iron deficiency"    NA                   "iron deficiency"   
#> [13840] "iron deficiency"    "no iron deficiency" NA                  
#> [13843] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13846] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13849] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13852] NA                   NA                   NA                  
#> [13855] "no iron deficiency" "iron deficiency"    NA                  
#> [13858] NA                   "iron deficiency"    "no iron deficiency"
#> [13861] NA                   "no iron deficiency" "no iron deficiency"
#> [13864] NA                   NA                   "no iron deficiency"
#> [13867] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13870] NA                   NA                   "no iron deficiency"
#> [13873] NA                   "no iron deficiency" "iron deficiency"   
#> [13876] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13879] NA                   NA                   NA                  
#> [13882] NA                   "no iron deficiency" NA                  
#> [13885] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [13888] NA                   NA                   "iron deficiency"   
#> [13891] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13894] "iron deficiency"    NA                   NA                  
#> [13897] NA                   "no iron deficiency" "no iron deficiency"
#> [13900] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13903] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13906] NA                   "iron deficiency"    "no iron deficiency"
#> [13909] "no iron deficiency" NA                   "no iron deficiency"
#> [13912] "iron deficiency"    NA                   "no iron deficiency"
#> [13915] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13918] NA                   NA                   "iron deficiency"   
#> [13921] NA                   "no iron deficiency" "no iron deficiency"
#> [13924] "no iron deficiency" NA                   NA                  
#> [13927] "no iron deficiency" NA                   NA                  
#> [13930] "iron deficiency"    NA                   "no iron deficiency"
#> [13933] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [13936] "no iron deficiency" NA                   NA                  
#> [13939] NA                   NA                   "iron deficiency"   
#> [13942] NA                   "no iron deficiency" NA                  
#> [13945] "no iron deficiency" "no iron deficiency" NA                  
#> [13948] "no iron deficiency" NA                   "no iron deficiency"
#> [13951] "no iron deficiency" NA                   NA                  
#> [13954] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13957] NA                   NA                   NA                  
#> [13960] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [13963] "no iron deficiency" "iron deficiency"    NA                  
#> [13966] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [13969] "no iron deficiency" "iron deficiency"    NA                  
#> [13972] "iron deficiency"    "iron deficiency"    NA                  
#> [13975] NA                   "no iron deficiency" "no iron deficiency"
#> [13978] "no iron deficiency" NA                   NA                  
#> [13981] "no iron deficiency" "iron deficiency"    NA                  
#> [13984] NA                   "no iron deficiency" NA                  
#> [13987] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [13990] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13993] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [13996] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [13999] "iron deficiency"    NA                   "iron deficiency"   
#> [14002] NA                   "no iron deficiency" "no iron deficiency"
#> [14005] NA                   "iron deficiency"    "iron deficiency"   
#> [14008] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14011] NA                   NA                   "no iron deficiency"
#> [14014] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14017] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14020] "no iron deficiency" NA                   "no iron deficiency"
#> [14023] "iron deficiency"    "iron deficiency"    NA                  
#> [14026] NA                   NA                   "no iron deficiency"
#> [14029] "iron deficiency"    "iron deficiency"    NA                  
#> [14032] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14035] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14038] "no iron deficiency" NA                   "no iron deficiency"
#> [14041] NA                   "iron deficiency"    NA                  
#> [14044] "iron deficiency"    NA                   NA                  
#> [14047] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14050] "iron deficiency"    "iron deficiency"    NA                  
#> [14053] NA                   NA                   "iron deficiency"   
#> [14056] "iron deficiency"    "iron deficiency"    NA                  
#> [14059] "iron deficiency"    "iron deficiency"    NA                  
#> [14062] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14065] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14068] "iron deficiency"    NA                   "iron deficiency"   
#> [14071] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14074] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14077] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14080] "iron deficiency"    NA                   NA                  
#> [14083] NA                   NA                   "no iron deficiency"
#> [14086] NA                   "iron deficiency"    "iron deficiency"   
#> [14089] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14092] NA                   "iron deficiency"    "no iron deficiency"
#> [14095] NA                   "iron deficiency"    "iron deficiency"   
#> [14098] "iron deficiency"    NA                   NA                  
#> [14101] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14104] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14107] NA                   "iron deficiency"    "iron deficiency"   
#> [14110] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14113] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14116] "iron deficiency"    "iron deficiency"    NA                  
#> [14119] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14122] NA                   NA                   "no iron deficiency"
#> [14125] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14128] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14131] "iron deficiency"    "no iron deficiency" NA                  
#> [14134] "no iron deficiency" NA                   "no iron deficiency"
#> [14137] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14140] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14143] "iron deficiency"    NA                   "iron deficiency"   
#> [14146] "iron deficiency"    "iron deficiency"    NA                  
#> [14149] NA                   NA                   NA                  
#> [14152] NA                   "iron deficiency"    "iron deficiency"   
#> [14155] NA                   "iron deficiency"    "iron deficiency"   
#> [14158] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14161] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14164] NA                   "iron deficiency"    "iron deficiency"   
#> [14167] "iron deficiency"    "no iron deficiency" NA                  
#> [14170] NA                   "iron deficiency"    "iron deficiency"   
#> [14173] "iron deficiency"    NA                   "iron deficiency"   
#> [14176] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14179] NA                   "no iron deficiency" "no iron deficiency"
#> [14182] "no iron deficiency" NA                   "no iron deficiency"
#> [14185] NA                   "iron deficiency"    "no iron deficiency"
#> [14188] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14191] "iron deficiency"    NA                   NA                  
#> [14194] NA                   NA                   "iron deficiency"   
#> [14197] NA                   "no iron deficiency" NA                  
#> [14200] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14203] "no iron deficiency" NA                   "no iron deficiency"
#> [14206] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14209] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14212] NA                   NA                   "no iron deficiency"
#> [14215] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14218] "no iron deficiency" NA                   NA                  
#> [14221] "iron deficiency"    NA                   "no iron deficiency"
#> [14224] NA                   "iron deficiency"    "no iron deficiency"
#> [14227] NA                   "iron deficiency"    NA                  
#> [14230] NA                   "no iron deficiency" "iron deficiency"   
#> [14233] "iron deficiency"    NA                   NA                  
#> [14236] "iron deficiency"    "no iron deficiency" NA                  
#> [14239] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14242] NA                   "iron deficiency"    "iron deficiency"   
#> [14245] "iron deficiency"    "iron deficiency"    NA                  
#> [14248] "no iron deficiency" NA                   "iron deficiency"   
#> [14251] "iron deficiency"    "no iron deficiency" NA                  
#> [14254] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14257] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14260] "no iron deficiency" NA                   "iron deficiency"   
#> [14263] "no iron deficiency" NA                   "iron deficiency"   
#> [14266] "no iron deficiency" "iron deficiency"    NA                  
#> [14269] "iron deficiency"    "no iron deficiency" NA                  
#> [14272] NA                   NA                   "iron deficiency"   
#> [14275] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14278] "no iron deficiency" NA                   "iron deficiency"   
#> [14281] NA                   "no iron deficiency" "no iron deficiency"
#> [14284] "iron deficiency"    NA                   NA                  
#> [14287] "iron deficiency"    "iron deficiency"    NA                  
#> [14290] NA                   "iron deficiency"    "iron deficiency"   
#> [14293] "no iron deficiency" "no iron deficiency" NA                  
#> [14296] "iron deficiency"    NA                   NA                  
#> [14299] NA                   "iron deficiency"    "iron deficiency"   
#> [14302] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14305] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14308] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14311] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14314] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14317] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14320] "iron deficiency"    NA                   NA                  
#> [14323] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14326] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14329] "iron deficiency"    "iron deficiency"    NA                  
#> [14332] "iron deficiency"    NA                   NA                  
#> [14335] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14338] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14341] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14344] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14347] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14350] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14353] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14356] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14359] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14362] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14365] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14368] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14371] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14374] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14377] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14380] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14383] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14386] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14389] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14392] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14395] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14398] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14401] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14404] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14407] "no iron deficiency" NA                   "iron deficiency"   
#> [14410] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14413] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14416] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14419] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14422] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14425] "no iron deficiency" NA                   "iron deficiency"   
#> [14428] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14431] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14434] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14437] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14440] NA                   "iron deficiency"    "iron deficiency"   
#> [14443] "no iron deficiency" "no iron deficiency" NA                  
#> [14446] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14449] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14452] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14455] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14458] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14461] NA                   "no iron deficiency" "no iron deficiency"
#> [14464] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14467] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14470] "no iron deficiency" NA                   "no iron deficiency"
#> [14473] "iron deficiency"    NA                   NA                  
#> [14476] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14479] "iron deficiency"    "iron deficiency"    NA                  
#> [14482] "iron deficiency"    NA                   "no iron deficiency"
#> [14485] "no iron deficiency" NA                   NA                  
#> [14488] NA                   "no iron deficiency" "no iron deficiency"
#> [14491] NA                   NA                   NA                  
#> [14494] NA                   "iron deficiency"    NA                  
#> [14497] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14500] "iron deficiency"    NA                   "no iron deficiency"
#> [14503] NA                   "iron deficiency"    "iron deficiency"   
#> [14506] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14509] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14512] "iron deficiency"    NA                   NA                  
#> [14515] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14518] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14521] "no iron deficiency" "no iron deficiency" NA                  
#> [14524] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14527] "no iron deficiency" NA                   "no iron deficiency"
#> [14530] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14533] "no iron deficiency" NA                   "iron deficiency"   
#> [14536] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14539] NA                   NA                   NA                  
#> [14542] NA                   NA                   "no iron deficiency"
#> [14545] NA                   NA                   NA                  
#> [14548] NA                   NA                   "iron deficiency"   
#> [14551] NA                   "iron deficiency"    NA                  
#> [14554] "iron deficiency"    NA                   "iron deficiency"   
#> [14557] "iron deficiency"    NA                   NA                  
#> [14560] NA                   "iron deficiency"    "iron deficiency"   
#> [14563] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14566] NA                   NA                   "no iron deficiency"
#> [14569] NA                   "no iron deficiency" "no iron deficiency"
#> [14572] NA                   NA                   NA                  
#> [14575] NA                   NA                   NA                  
#> [14578] "iron deficiency"    "iron deficiency"    NA                  
#> [14581] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14584] NA                   "no iron deficiency" "no iron deficiency"
#> [14587] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14590] "iron deficiency"    NA                   "no iron deficiency"
#> [14593] NA                   NA                   "no iron deficiency"
#> [14596] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14599] "no iron deficiency" "no iron deficiency" NA                  
#> [14602] "no iron deficiency" "iron deficiency"    NA                  
#> [14605] "no iron deficiency" NA                   NA                  
#> [14608] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14611] NA                   "no iron deficiency" NA                  
#> [14614] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14617] "iron deficiency"    "no iron deficiency" NA                  
#> [14620] "no iron deficiency" "iron deficiency"    NA                  
#> [14623] "no iron deficiency" NA                   "no iron deficiency"
#> [14626] NA                   NA                   "no iron deficiency"
#> [14629] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14632] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14635] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14638] NA                   "iron deficiency"    "iron deficiency"   
#> [14641] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14644] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14647] "no iron deficiency" NA                   "iron deficiency"   
#> [14650] NA                   "no iron deficiency" "no iron deficiency"
#> [14653] NA                   NA                   "no iron deficiency"
#> [14656] NA                   NA                   NA                  
#> [14659] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14662] NA                   "no iron deficiency" NA                  
#> [14665] NA                   NA                   "no iron deficiency"
#> [14668] NA                   "iron deficiency"    NA                  
#> [14671] "no iron deficiency" "iron deficiency"    NA                  
#> [14674] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14677] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14680] "iron deficiency"    NA                   "no iron deficiency"
#> [14683] "iron deficiency"    NA                   "iron deficiency"   
#> [14686] NA                   NA                   "iron deficiency"   
#> [14689] NA                   "iron deficiency"    "no iron deficiency"
#> [14692] "iron deficiency"    NA                   "no iron deficiency"
#> [14695] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14698] NA                   NA                   "iron deficiency"   
#> [14701] "iron deficiency"    "no iron deficiency" NA                  
#> [14704] NA                   "no iron deficiency" NA                  
#> [14707] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14710] "iron deficiency"    NA                   "no iron deficiency"
#> [14713] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14716] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14719] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14722] "no iron deficiency" NA                   NA                  
#> [14725] "no iron deficiency" "iron deficiency"    NA                  
#> [14728] "iron deficiency"    NA                   "no iron deficiency"
#> [14731] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14734] "iron deficiency"    "no iron deficiency" NA                  
#> [14737] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14740] "no iron deficiency" NA                   "no iron deficiency"
#> [14743] NA                   NA                   "iron deficiency"   
#> [14746] NA                   "no iron deficiency" NA                  
#> [14749] NA                   "no iron deficiency" "iron deficiency"   
#> [14752] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14755] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14758] "no iron deficiency" NA                   "no iron deficiency"
#> [14761] "no iron deficiency" NA                   "no iron deficiency"
#> [14764] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14767] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14770] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14773] NA                   NA                   "iron deficiency"   
#> [14776] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14779] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [14782] NA                   "iron deficiency"    "no iron deficiency"
#> [14785] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14788] NA                   "iron deficiency"    "iron deficiency"   
#> [14791] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14794] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14797] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [14800] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14803] NA                   "no iron deficiency" "no iron deficiency"
#> [14806] "iron deficiency"    NA                   "iron deficiency"   
#> [14809] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14812] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14815] "iron deficiency"    "iron deficiency"    NA                  
#> [14818] NA                   "no iron deficiency" "no iron deficiency"
#> [14821] "no iron deficiency" NA                   "no iron deficiency"
#> [14824] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14827] NA                   NA                   NA                  
#> [14830] NA                   "iron deficiency"    NA                  
#> [14833] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14836] "no iron deficiency" NA                   NA                  
#> [14839] "no iron deficiency" "no iron deficiency" NA                  
#> [14842] "iron deficiency"    NA                   NA                  
#> [14845] NA                   NA                   NA                  
#> [14848] NA                   "no iron deficiency" "no iron deficiency"
#> [14851] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14854] "no iron deficiency" "iron deficiency"    NA                  
#> [14857] NA                   "no iron deficiency" NA                  
#> [14860] "no iron deficiency" NA                   "iron deficiency"   
#> [14863] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14866] "no iron deficiency" NA                   NA                  
#> [14869] NA                   NA                   "no iron deficiency"
#> [14872] "iron deficiency"    NA                   NA                  
#> [14875] "no iron deficiency" NA                   "no iron deficiency"
#> [14878] "no iron deficiency" NA                   "no iron deficiency"
#> [14881] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14884] NA                   "no iron deficiency" NA                  
#> [14887] NA                   "no iron deficiency" NA                  
#> [14890] NA                   NA                   "no iron deficiency"
#> [14893] NA                   "no iron deficiency" "no iron deficiency"
#> [14896] NA                   NA                   NA                  
#> [14899] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14902] "no iron deficiency" NA                   "no iron deficiency"
#> [14905] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14908] "iron deficiency"    NA                   "no iron deficiency"
#> [14911] NA                   NA                   NA                  
#> [14914] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14917] NA                   "iron deficiency"    "no iron deficiency"
#> [14920] NA                   "iron deficiency"    NA                  
#> [14923] NA                   "no iron deficiency" NA                  
#> [14926] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14929] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14932] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14935] NA                   "no iron deficiency" "no iron deficiency"
#> [14938] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14941] NA                   NA                   NA                  
#> [14944] "no iron deficiency" NA                   NA                  
#> [14947] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14950] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14953] NA                   "iron deficiency"    NA                  
#> [14956] "no iron deficiency" NA                   "iron deficiency"   
#> [14959] NA                   "no iron deficiency" "iron deficiency"   
#> [14962] "iron deficiency"    NA                   "no iron deficiency"
#> [14965] "no iron deficiency" "no iron deficiency" NA                  
#> [14968] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [14971] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [14974] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14977] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [14980] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [14983] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [14986] NA                   "no iron deficiency" NA                  
#> [14989] "iron deficiency"    "iron deficiency"    NA                  
#> [14992] "no iron deficiency" NA                   NA                  
#> [14995] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [14998] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15001] NA                   "iron deficiency"    "iron deficiency"   
#> [15004] NA                   "no iron deficiency" "no iron deficiency"
#> [15007] NA                   "no iron deficiency" "no iron deficiency"
#> [15010] "iron deficiency"    NA                   NA                  
#> [15013] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15016] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15019] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15022] NA                   NA                   NA                  
#> [15025] "iron deficiency"    NA                   NA                  
#> [15028] "iron deficiency"    NA                   "no iron deficiency"
#> [15031] "iron deficiency"    "no iron deficiency" NA                  
#> [15034] NA                   "iron deficiency"    "iron deficiency"   
#> [15037] NA                   NA                   NA                  
#> [15040] NA                   "no iron deficiency" "iron deficiency"   
#> [15043] "iron deficiency"    "iron deficiency"    NA                  
#> [15046] NA                   NA                   NA                  
#> [15049] "iron deficiency"    NA                   NA                  
#> [15052] "no iron deficiency" NA                   "no iron deficiency"
#> [15055] NA                   NA                   NA                  
#> [15058] NA                   NA                   "no iron deficiency"
#> [15061] "no iron deficiency" NA                   "iron deficiency"   
#> [15064] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15067] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15070] NA                   "no iron deficiency" "no iron deficiency"
#> [15073] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15076] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15079] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15082] NA                   NA                   "iron deficiency"   
#> [15085] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15088] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15091] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15094] "iron deficiency"    "no iron deficiency" NA                  
#> [15097] "iron deficiency"    "no iron deficiency" NA                  
#> [15100] "no iron deficiency" "no iron deficiency" NA                  
#> [15103] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15106] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15109] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15112] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15115] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15118] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15121] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15124] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15127] NA                   NA                   "no iron deficiency"
#> [15130] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15133] NA                   "no iron deficiency" "iron deficiency"   
#> [15136] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15139] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15142] NA                   "iron deficiency"    "iron deficiency"   
#> [15145] NA                   "no iron deficiency" "no iron deficiency"
#> [15148] "iron deficiency"    "no iron deficiency" NA                  
#> [15151] NA                   "iron deficiency"    "no iron deficiency"
#> [15154] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15157] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15160] "no iron deficiency" "no iron deficiency" NA                  
#> [15163] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15166] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15169] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15172] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15175] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15178] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15181] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15184] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15187] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15190] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15193] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15196] NA                   "iron deficiency"    "no iron deficiency"
#> [15199] "no iron deficiency" NA                   "iron deficiency"   
#> [15202] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15205] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15208] "no iron deficiency" "no iron deficiency" NA                  
#> [15211] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15214] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15217] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15220] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15223] "no iron deficiency" NA                   "iron deficiency"   
#> [15226] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15229] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15232] "iron deficiency"    "iron deficiency"    NA                  
#> [15235] "no iron deficiency" "iron deficiency"    NA                  
#> [15238] NA                   NA                   "iron deficiency"   
#> [15241] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15244] NA                   "no iron deficiency" "no iron deficiency"
#> [15247] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15250] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15253] "no iron deficiency" NA                   "no iron deficiency"
#> [15256] "no iron deficiency" "no iron deficiency" NA                  
#> [15259] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15262] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15265] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15268] "iron deficiency"    NA                   "iron deficiency"   
#> [15271] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15274] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15277] "iron deficiency"    "iron deficiency"    NA                  
#> [15280] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15283] "no iron deficiency" NA                   NA                  
#> [15286] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15289] NA                   "no iron deficiency" "no iron deficiency"
#> [15292] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15295] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15298] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15301] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15304] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15307] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15310] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15313] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15316] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15319] NA                   NA                   "iron deficiency"   
#> [15322] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15325] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15328] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15331] NA                   "iron deficiency"    "no iron deficiency"
#> [15334] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15337] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15340] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15343] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15346] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15349] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15352] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15355] "iron deficiency"    "no iron deficiency" NA                  
#> [15358] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15361] "iron deficiency"    "iron deficiency"    NA                  
#> [15364] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15367] NA                   "no iron deficiency" "iron deficiency"   
#> [15370] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15373] NA                   "no iron deficiency" NA                  
#> [15376] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15379] "iron deficiency"    NA                   "no iron deficiency"
#> [15382] "iron deficiency"    NA                   "iron deficiency"   
#> [15385] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15388] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15391] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15394] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15397] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15400] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15403] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15406] NA                   "no iron deficiency" "no iron deficiency"
#> [15409] "iron deficiency"    NA                   NA                  
#> [15412] "iron deficiency"    NA                   NA                  
#> [15415] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15418] NA                   "iron deficiency"    "iron deficiency"   
#> [15421] "no iron deficiency" "no iron deficiency" NA                  
#> [15424] NA                   NA                   "iron deficiency"   
#> [15427] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15430] NA                   "iron deficiency"    NA                  
#> [15433] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15436] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15439] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15442] "iron deficiency"    "iron deficiency"    NA                  
#> [15445] NA                   "iron deficiency"    "iron deficiency"   
#> [15448] NA                   "no iron deficiency" "iron deficiency"   
#> [15451] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15454] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15457] NA                   "no iron deficiency" "no iron deficiency"
#> [15460] "iron deficiency"    NA                   "no iron deficiency"
#> [15463] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15466] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15469] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15472] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15475] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15478] NA                   NA                   "no iron deficiency"
#> [15481] "no iron deficiency" NA                   "no iron deficiency"
#> [15484] "iron deficiency"    "iron deficiency"    NA                  
#> [15487] "no iron deficiency" NA                   "iron deficiency"   
#> [15490] NA                   NA                   "no iron deficiency"
#> [15493] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15496] NA                   "iron deficiency"    "no iron deficiency"
#> [15499] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15502] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15505] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15508] NA                   "iron deficiency"    "no iron deficiency"
#> [15511] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15514] "iron deficiency"    NA                   NA                  
#> [15517] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15520] NA                   "iron deficiency"    "no iron deficiency"
#> [15523] NA                   "iron deficiency"    "iron deficiency"   
#> [15526] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15529] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15532] "no iron deficiency" "no iron deficiency" NA                  
#> [15535] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15538] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15541] "no iron deficiency" "no iron deficiency" NA                  
#> [15544] "iron deficiency"    "iron deficiency"    NA                  
#> [15547] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15550] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15553] "iron deficiency"    NA                   NA                  
#> [15556] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15559] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15562] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15565] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15568] "iron deficiency"    NA                   "iron deficiency"   
#> [15571] "no iron deficiency" NA                   "iron deficiency"   
#> [15574] NA                   "no iron deficiency" "iron deficiency"   
#> [15577] NA                   "iron deficiency"    "no iron deficiency"
#> [15580] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15583] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15586] "no iron deficiency" NA                   "no iron deficiency"
#> [15589] NA                   NA                   "no iron deficiency"
#> [15592] "iron deficiency"    "no iron deficiency" NA                  
#> [15595] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15598] NA                   NA                   NA                  
#> [15601] NA                   "no iron deficiency" NA                  
#> [15604] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15607] "no iron deficiency" NA                   "iron deficiency"   
#> [15610] "iron deficiency"    NA                   "iron deficiency"   
#> [15613] NA                   "no iron deficiency" "no iron deficiency"
#> [15616] NA                   "iron deficiency"    "iron deficiency"   
#> [15619] "no iron deficiency" "iron deficiency"    NA                  
#> [15622] "no iron deficiency" NA                   "no iron deficiency"
#> [15625] "no iron deficiency" NA                   NA                  
#> [15628] NA                   "no iron deficiency" "no iron deficiency"
#> [15631] "no iron deficiency" NA                   "no iron deficiency"
#> [15634] "no iron deficiency" NA                   "no iron deficiency"
#> [15637] "iron deficiency"    NA                   "no iron deficiency"
#> [15640] "no iron deficiency" NA                   NA                  
#> [15643] NA                   NA                   "iron deficiency"   
#> [15646] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15649] NA                   "no iron deficiency" "iron deficiency"   
#> [15652] "iron deficiency"    "no iron deficiency" NA                  
#> [15655] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15658] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15661] "no iron deficiency" NA                   NA                  
#> [15664] "iron deficiency"    NA                   "iron deficiency"   
#> [15667] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15670] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15673] NA                   "no iron deficiency" "iron deficiency"   
#> [15676] NA                   NA                   "no iron deficiency"
#> [15679] "iron deficiency"    NA                   "iron deficiency"   
#> [15682] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15685] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15688] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15691] NA                   NA                   "no iron deficiency"
#> [15694] "iron deficiency"    "no iron deficiency" NA                  
#> [15697] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15700] "iron deficiency"    NA                   "no iron deficiency"
#> [15703] NA                   NA                   "iron deficiency"   
#> [15706] NA                   "no iron deficiency" "iron deficiency"   
#> [15709] "no iron deficiency" NA                   "no iron deficiency"
#> [15712] NA                   "iron deficiency"    "iron deficiency"   
#> [15715] "iron deficiency"    NA                   NA                  
#> [15718] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15721] NA                   "iron deficiency"    NA                  
#> [15724] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15727] NA                   "no iron deficiency" "no iron deficiency"
#> [15730] "iron deficiency"    NA                   NA                  
#> [15733] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15736] NA                   "iron deficiency"    NA                  
#> [15739] NA                   "iron deficiency"    "no iron deficiency"
#> [15742] "no iron deficiency" NA                   NA                  
#> [15745] "no iron deficiency" "no iron deficiency" NA                  
#> [15748] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [15751] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15754] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15757] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15760] "no iron deficiency" "no iron deficiency" NA                  
#> [15763] NA                   NA                   NA                  
#> [15766] "iron deficiency"    NA                   "iron deficiency"   
#> [15769] "no iron deficiency" "iron deficiency"    NA                  
#> [15772] "no iron deficiency" NA                   "no iron deficiency"
#> [15775] NA                   "iron deficiency"    "no iron deficiency"
#> [15778] NA                   "no iron deficiency" NA                  
#> [15781] "iron deficiency"    "iron deficiency"    NA                  
#> [15784] NA                   NA                   NA                  
#> [15787] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15790] NA                   "no iron deficiency" "iron deficiency"   
#> [15793] NA                   "iron deficiency"    "no iron deficiency"
#> [15796] "no iron deficiency" NA                   "iron deficiency"   
#> [15799] "iron deficiency"    "no iron deficiency" NA                  
#> [15802] "iron deficiency"    "iron deficiency"    NA                  
#> [15805] "no iron deficiency" NA                   NA                  
#> [15808] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15811] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15814] NA                   "no iron deficiency" NA                  
#> [15817] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15820] NA                   "no iron deficiency" "no iron deficiency"
#> [15823] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15826] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15829] NA                   "iron deficiency"    "iron deficiency"   
#> [15832] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15835] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15838] "iron deficiency"    NA                   "iron deficiency"   
#> [15841] NA                   "iron deficiency"    "iron deficiency"   
#> [15844] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15847] "no iron deficiency" "iron deficiency"    NA                  
#> [15850] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [15853] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15856] "no iron deficiency" NA                   "iron deficiency"   
#> [15859] "iron deficiency"    "iron deficiency"    NA                  
#> [15862] "iron deficiency"    "iron deficiency"    NA                  
#> [15865] "no iron deficiency" "iron deficiency"    NA                  
#> [15868] NA                   "iron deficiency"    "iron deficiency"   
#> [15871] "iron deficiency"    NA                   NA                  
#> [15874] NA                   "iron deficiency"    NA                  
#> [15877] NA                   "no iron deficiency" "iron deficiency"   
#> [15880] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15883] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [15886] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15889] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15892] NA                   NA                   "iron deficiency"   
#> [15895] NA                   NA                   "no iron deficiency"
#> [15898] NA                   "no iron deficiency" NA                  
#> [15901] "iron deficiency"    "no iron deficiency" NA                  
#> [15904] NA                   "iron deficiency"    NA                  
#> [15907] NA                   "iron deficiency"    NA                  
#> [15910] NA                   NA                   "iron deficiency"   
#> [15913] "iron deficiency"    NA                   "iron deficiency"   
#> [15916] NA                   NA                   "iron deficiency"   
#> [15919] NA                   "no iron deficiency" "iron deficiency"   
#> [15922] NA                   "iron deficiency"    "iron deficiency"   
#> [15925] "iron deficiency"    NA                   "iron deficiency"   
#> [15928] "iron deficiency"    "iron deficiency"    NA                  
#> [15931] "iron deficiency"    "no iron deficiency" NA                  
#> [15934] "no iron deficiency" NA                   "iron deficiency"   
#> [15937] "no iron deficiency" NA                   NA                  
#> [15940] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [15943] "iron deficiency"    NA                   "iron deficiency"   
#> [15946] "iron deficiency"    NA                   NA                  
#> [15949] "no iron deficiency" NA                   "no iron deficiency"
#> [15952] NA                   NA                   NA                  
#> [15955] NA                   NA                   NA                  
#> [15958] "no iron deficiency" NA                   NA                  
#> [15961] NA                   NA                   "iron deficiency"   
#> [15964] "no iron deficiency" NA                   "no iron deficiency"
#> [15967] NA                   "iron deficiency"    NA                  
#> [15970] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [15973] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [15976] "no iron deficiency" "iron deficiency"    NA                  
#> [15979] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15982] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [15985] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [15988] "iron deficiency"    "no iron deficiency" NA                  
#> [15991] "no iron deficiency" NA                   "iron deficiency"   
#> [15994] "no iron deficiency" NA                   NA                  
#> [15997] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16000] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16003] "no iron deficiency" NA                   "no iron deficiency"
#> [16006] NA                   "iron deficiency"    "no iron deficiency"
#> [16009] "iron deficiency"    "iron deficiency"    NA                  
#> [16012] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16015] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16018] "no iron deficiency" NA                   "no iron deficiency"
#> [16021] "iron deficiency"    NA                   "iron deficiency"   
#> [16024] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16027] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16030] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16033] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16036] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16039] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16042] "iron deficiency"    "iron deficiency"    NA                  
#> [16045] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16048] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16051] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16054] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16057] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16060] "no iron deficiency" NA                   "iron deficiency"   
#> [16063] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16066] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16069] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16072] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16075] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16078] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16081] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16084] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16087] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16090] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16093] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16096] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16099] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16102] "iron deficiency"    "no iron deficiency" NA                  
#> [16105] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16108] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16111] "iron deficiency"    NA                   "iron deficiency"   
#> [16114] "iron deficiency"    "iron deficiency"    NA                  
#> [16117] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16120] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16123] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16126] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16129] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16132] "iron deficiency"    NA                   "iron deficiency"   
#> [16135] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16138] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16141] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16144] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16147] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16150] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16153] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16156] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16159] "iron deficiency"    NA                   "no iron deficiency"
#> [16162] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16165] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16168] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16171] NA                   "no iron deficiency" "no iron deficiency"
#> [16174] "iron deficiency"    NA                   "iron deficiency"   
#> [16177] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16180] "iron deficiency"    "no iron deficiency" NA                  
#> [16183] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16186] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16189] "no iron deficiency" "no iron deficiency" NA                  
#> [16192] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16195] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16198] "no iron deficiency" "iron deficiency"    NA                  
#> [16201] "iron deficiency"    NA                   NA                  
#> [16204] "no iron deficiency" "no iron deficiency" NA                  
#> [16207] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16210] "no iron deficiency" NA                   "no iron deficiency"
#> [16213] "iron deficiency"    NA                   "iron deficiency"   
#> [16216] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16219] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16222] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16225] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16228] NA                   "iron deficiency"    "no iron deficiency"
#> [16231] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16234] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16237] "iron deficiency"    NA                   "iron deficiency"   
#> [16240] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16243] "no iron deficiency" "iron deficiency"    NA                  
#> [16246] "no iron deficiency" NA                   "iron deficiency"   
#> [16249] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16252] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16255] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16258] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16261] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16264] "iron deficiency"    NA                   "iron deficiency"   
#> [16267] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16270] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16273] "iron deficiency"    "iron deficiency"    NA                  
#> [16276] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16279] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16282] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16285] "no iron deficiency" NA                   "no iron deficiency"
#> [16288] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16291] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16294] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16297] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16300] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16303] "iron deficiency"    NA                   "no iron deficiency"
#> [16306] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16309] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16312] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16315] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16318] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16321] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16324] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16327] "iron deficiency"    "no iron deficiency" NA                  
#> [16330] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16333] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16336] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16339] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16342] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16345] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16348] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16351] NA                   "iron deficiency"    "iron deficiency"   
#> [16354] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16357] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16360] "no iron deficiency" NA                   "no iron deficiency"
#> [16363] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16366] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16369] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16372] "no iron deficiency" NA                   "no iron deficiency"
#> [16375] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16378] NA                   "no iron deficiency" "iron deficiency"   
#> [16381] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16384] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16387] "no iron deficiency" NA                   "no iron deficiency"
#> [16390] "no iron deficiency" NA                   "iron deficiency"   
#> [16393] "no iron deficiency" NA                   NA                  
#> [16396] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16399] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16402] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16405] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16408] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16411] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16414] "no iron deficiency" NA                   "no iron deficiency"
#> [16417] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16420] "no iron deficiency" NA                   "no iron deficiency"
#> [16423] NA                   "iron deficiency"    "iron deficiency"   
#> [16426] "iron deficiency"    NA                   NA                  
#> [16429] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16432] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16435] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16438] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16441] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16444] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16447] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16450] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16453] "iron deficiency"    "iron deficiency"    NA                  
#> [16456] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16459] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16462] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16465] NA                   "no iron deficiency" "iron deficiency"   
#> [16468] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16471] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16474] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16477] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16480] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16483] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16486] NA                   "iron deficiency"    "no iron deficiency"
#> [16489] "iron deficiency"    "no iron deficiency" NA                  
#> [16492] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16495] NA                   "iron deficiency"    "no iron deficiency"
#> [16498] "iron deficiency"    NA                   "iron deficiency"   
#> [16501] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16504] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16507] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16510] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16513] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16516] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16519] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16522] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16525] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16528] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16531] NA                   "no iron deficiency" "iron deficiency"   
#> [16534] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16537] "no iron deficiency" NA                   NA                  
#> [16540] "no iron deficiency" "no iron deficiency" NA                  
#> [16543] "no iron deficiency" "iron deficiency"    NA                  
#> [16546] "iron deficiency"    NA                   "iron deficiency"   
#> [16549] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16552] "no iron deficiency" NA                   "no iron deficiency"
#> [16555] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16558] "iron deficiency"    NA                   "no iron deficiency"
#> [16561] NA                   NA                   NA                  
#> [16564] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16567] NA                   "no iron deficiency" "iron deficiency"   
#> [16570] NA                   "iron deficiency"    "no iron deficiency"
#> [16573] "iron deficiency"    NA                   NA                  
#> [16576] NA                   "iron deficiency"    "iron deficiency"   
#> [16579] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16582] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16585] NA                   "no iron deficiency" "no iron deficiency"
#> [16588] "iron deficiency"    NA                   NA                  
#> [16591] "iron deficiency"    NA                   NA                  
#> [16594] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16597] "no iron deficiency" NA                   NA                  
#> [16600] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16603] "iron deficiency"    "no iron deficiency" NA                  
#> [16606] NA                   "iron deficiency"    NA                  
#> [16609] "no iron deficiency" "no iron deficiency" NA                  
#> [16612] "no iron deficiency" "no iron deficiency" NA                  
#> [16615] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16618] NA                   NA                   NA                  
#> [16621] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16624] "no iron deficiency" "no iron deficiency" NA                  
#> [16627] "no iron deficiency" NA                   NA                  
#> [16630] "iron deficiency"    "iron deficiency"    NA                  
#> [16633] "iron deficiency"    NA                   "no iron deficiency"
#> [16636] "iron deficiency"    NA                   "no iron deficiency"
#> [16639] "iron deficiency"    "iron deficiency"    NA                  
#> [16642] "no iron deficiency" NA                   "iron deficiency"   
#> [16645] "iron deficiency"    "iron deficiency"    NA                  
#> [16648] "no iron deficiency" "iron deficiency"    NA                  
#> [16651] "no iron deficiency" NA                   NA                  
#> [16654] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16657] NA                   "no iron deficiency" "no iron deficiency"
#> [16660] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16663] NA                   NA                   "no iron deficiency"
#> [16666] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16669] NA                   "iron deficiency"    "no iron deficiency"
#> [16672] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16675] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16678] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16681] NA                   "iron deficiency"    "no iron deficiency"
#> [16684] "no iron deficiency" "iron deficiency"    NA                  
#> [16687] "no iron deficiency" NA                   NA                  
#> [16690] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16693] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16696] "iron deficiency"    NA                   "no iron deficiency"
#> [16699] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16702] NA                   "iron deficiency"    "iron deficiency"   
#> [16705] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16708] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16711] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16714] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16717] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16720] "iron deficiency"    "no iron deficiency" NA                  
#> [16723] NA                   "iron deficiency"    NA                  
#> [16726] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16729] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16732] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16735] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16738] NA                   "no iron deficiency" "iron deficiency"   
#> [16741] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16744] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16747] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16750] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16753] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16756] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16759] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16762] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16765] "no iron deficiency" NA                   "no iron deficiency"
#> [16768] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16771] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16774] "no iron deficiency" NA                   "no iron deficiency"
#> [16777] "no iron deficiency" "no iron deficiency" NA                  
#> [16780] NA                   "iron deficiency"    NA                  
#> [16783] NA                   "iron deficiency"    "no iron deficiency"
#> [16786] "no iron deficiency" NA                   "no iron deficiency"
#> [16789] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16792] "no iron deficiency" NA                   "no iron deficiency"
#> [16795] NA                   "no iron deficiency" NA                  
#> [16798] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16801] "no iron deficiency" NA                   "no iron deficiency"
#> [16804] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16807] NA                   "no iron deficiency" "iron deficiency"   
#> [16810] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16813] "no iron deficiency" "iron deficiency"    NA                  
#> [16816] "no iron deficiency" NA                   "iron deficiency"   
#> [16819] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16822] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16825] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16828] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16831] "no iron deficiency" NA                   "no iron deficiency"
#> [16834] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16837] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16840] NA                   "no iron deficiency" "iron deficiency"   
#> [16843] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16846] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16849] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16852] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16855] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16858] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16861] "iron deficiency"    "iron deficiency"    NA                  
#> [16864] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16867] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16870] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16873] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16876] NA                   "iron deficiency"    "no iron deficiency"
#> [16879] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16882] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16885] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16888] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16891] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16894] "no iron deficiency" NA                   "no iron deficiency"
#> [16897] "no iron deficiency" NA                   "no iron deficiency"
#> [16900] NA                   "iron deficiency"    NA                  
#> [16903] "iron deficiency"    "no iron deficiency" NA                  
#> [16906] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16909] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16912] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16915] "no iron deficiency" NA                   "no iron deficiency"
#> [16918] "iron deficiency"    "no iron deficiency" NA                  
#> [16921] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [16924] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16927] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16930] "no iron deficiency" NA                   "iron deficiency"   
#> [16933] "no iron deficiency" "no iron deficiency" NA                  
#> [16936] NA                   "iron deficiency"    "iron deficiency"   
#> [16939] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16942] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16945] NA                   NA                   "iron deficiency"   
#> [16948] "iron deficiency"    NA                   NA                  
#> [16951] NA                   "iron deficiency"    "no iron deficiency"
#> [16954] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16957] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16960] "iron deficiency"    "no iron deficiency" NA                  
#> [16963] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16966] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [16969] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [16972] NA                   "no iron deficiency" "no iron deficiency"
#> [16975] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [16978] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [16981] NA                   "no iron deficiency" "iron deficiency"   
#> [16984] "no iron deficiency" "iron deficiency"    NA                  
#> [16987] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [16990] "no iron deficiency" "no iron deficiency" NA                  
#> [16993] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [16996] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [16999] "iron deficiency"    "no iron deficiency" NA                  
#> [17002] "iron deficiency"    NA                   "no iron deficiency"
#> [17005] "no iron deficiency" NA                   NA                  
#> [17008] "no iron deficiency" NA                   "no iron deficiency"
#> [17011] "iron deficiency"    NA                   "no iron deficiency"
#> [17014] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17017] NA                   "iron deficiency"    "iron deficiency"   
#> [17020] NA                   "no iron deficiency" NA                  
#> [17023] NA                   NA                   "no iron deficiency"
#> [17026] "no iron deficiency" NA                   NA                  
#> [17029] NA                   NA                   "no iron deficiency"
#> [17032] NA                   "iron deficiency"    "no iron deficiency"
#> [17035] "no iron deficiency" "no iron deficiency" NA                  
#> [17038] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17041] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17044] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17047] "no iron deficiency" "iron deficiency"    NA                  
#> [17050] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17053] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17056] NA                   "no iron deficiency" "iron deficiency"   
#> [17059] "no iron deficiency" "no iron deficiency" NA                  
#> [17062] "no iron deficiency" NA                   NA                  
#> [17065] "no iron deficiency" NA                   NA                  
#> [17068] "no iron deficiency" "iron deficiency"    NA                  
#> [17071] "no iron deficiency" NA                   "iron deficiency"   
#> [17074] NA                   "iron deficiency"    "no iron deficiency"
#> [17077] "no iron deficiency" NA                   "no iron deficiency"
#> [17080] NA                   "no iron deficiency" "no iron deficiency"
#> [17083] NA                   "no iron deficiency" "iron deficiency"   
#> [17086] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17089] "no iron deficiency" "no iron deficiency" NA                  
#> [17092] NA                   "iron deficiency"    "iron deficiency"   
#> [17095] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17098] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17101] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17104] "iron deficiency"    NA                   NA                  
#> [17107] "no iron deficiency" NA                   NA                  
#> [17110] "no iron deficiency" NA                   "no iron deficiency"
#> [17113] NA                   "no iron deficiency" "no iron deficiency"
#> [17116] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17119] "iron deficiency"    NA                   "no iron deficiency"
#> [17122] NA                   "no iron deficiency" "no iron deficiency"
#> [17125] "no iron deficiency" "iron deficiency"    NA                  
#> [17128] NA                   NA                   "iron deficiency"   
#> [17131] "no iron deficiency" "no iron deficiency" NA                  
#> [17134] NA                   "no iron deficiency" "no iron deficiency"
#> [17137] "no iron deficiency" NA                   "no iron deficiency"
#> [17140] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17143] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17146] NA                   NA                   NA                  
#> [17149] NA                   "iron deficiency"    "iron deficiency"   
#> [17152] "no iron deficiency" "no iron deficiency" NA                  
#> [17155] "no iron deficiency" "no iron deficiency" NA                  
#> [17158] "no iron deficiency" "no iron deficiency" NA                  
#> [17161] "no iron deficiency" NA                   "no iron deficiency"
#> [17164] "no iron deficiency" NA                   "no iron deficiency"
#> [17167] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17170] "no iron deficiency" NA                   "no iron deficiency"
#> [17173] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17176] NA                   NA                   "no iron deficiency"
#> [17179] "no iron deficiency" "no iron deficiency" NA                  
#> [17182] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17185] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17188] "no iron deficiency" NA                   NA                  
#> [17191] NA                   "iron deficiency"    "no iron deficiency"
#> [17194] "no iron deficiency" NA                   NA                  
#> [17197] "no iron deficiency" "no iron deficiency" NA                  
#> [17200] "no iron deficiency" NA                   "no iron deficiency"
#> [17203] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17206] NA                   "no iron deficiency" NA                  
#> [17209] NA                   "no iron deficiency" "no iron deficiency"
#> [17212] "no iron deficiency" "iron deficiency"    NA                  
#> [17215] NA                   "no iron deficiency" "no iron deficiency"
#> [17218] "no iron deficiency" NA                   NA                  
#> [17221] "no iron deficiency" "no iron deficiency" NA                  
#> [17224] NA                   NA                   "iron deficiency"   
#> [17227] NA                   "no iron deficiency" "no iron deficiency"
#> [17230] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17233] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17236] "no iron deficiency" "no iron deficiency" NA                  
#> [17239] NA                   NA                   "no iron deficiency"
#> [17242] NA                   NA                   "iron deficiency"   
#> [17245] "iron deficiency"    "no iron deficiency" NA                  
#> [17248] "iron deficiency"    NA                   "no iron deficiency"
#> [17251] "no iron deficiency" "no iron deficiency" NA                  
#> [17254] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17257] "no iron deficiency" NA                   "iron deficiency"   
#> [17260] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17263] NA                   "iron deficiency"    NA                  
#> [17266] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17269] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17272] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17275] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17278] "iron deficiency"    "no iron deficiency" NA                  
#> [17281] NA                   "no iron deficiency" "iron deficiency"   
#> [17284] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17287] NA                   NA                   "iron deficiency"   
#> [17290] "iron deficiency"    "iron deficiency"    NA                  
#> [17293] "no iron deficiency" NA                   "iron deficiency"   
#> [17296] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17299] NA                   "no iron deficiency" NA                  
#> [17302] "no iron deficiency" NA                   "iron deficiency"   
#> [17305] "no iron deficiency" "no iron deficiency" NA                  
#> [17308] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17311] "no iron deficiency" "no iron deficiency" NA                  
#> [17314] NA                   "iron deficiency"    NA                  
#> [17317] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17320] NA                   NA                   NA                  
#> [17323] "no iron deficiency" "no iron deficiency" NA                  
#> [17326] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17329] NA                   "no iron deficiency" NA                  
#> [17332] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17335] "iron deficiency"    NA                   "iron deficiency"   
#> [17338] "iron deficiency"    "no iron deficiency" NA                  
#> [17341] NA                   "no iron deficiency" "iron deficiency"   
#> [17344] NA                   NA                   NA                  
#> [17347] NA                   "no iron deficiency" NA                  
#> [17350] NA                   NA                   "no iron deficiency"
#> [17353] "iron deficiency"    "no iron deficiency" NA                  
#> [17356] "iron deficiency"    NA                   NA                  
#> [17359] NA                   NA                   NA                  
#> [17362] NA                   NA                   "iron deficiency"   
#> [17365] "no iron deficiency" NA                   "no iron deficiency"
#> [17368] "no iron deficiency" "no iron deficiency" NA                  
#> [17371] NA                   "no iron deficiency" "no iron deficiency"
#> [17374] NA                   "no iron deficiency" NA                  
#> [17377] NA                   "no iron deficiency" NA                  
#> [17380] "iron deficiency"    NA                   "no iron deficiency"
#> [17383] "no iron deficiency" NA                   "no iron deficiency"
#> [17386] NA                   "no iron deficiency" "no iron deficiency"
#> [17389] NA                   NA                   "no iron deficiency"
#> [17392] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17395] "iron deficiency"    "no iron deficiency" NA                  
#> [17398] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17401] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17404] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17407] "no iron deficiency" NA                   NA                  
#> [17410] NA                   NA                   "no iron deficiency"
#> [17413] NA                   "iron deficiency"    "no iron deficiency"
#> [17416] "no iron deficiency" NA                   "no iron deficiency"
#> [17419] "iron deficiency"    NA                   NA                  
#> [17422] NA                   "no iron deficiency" NA                  
#> [17425] NA                   NA                   NA                  
#> [17428] NA                   "no iron deficiency" NA                  
#> [17431] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17434] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17437] NA                   NA                   "no iron deficiency"
#> [17440] "iron deficiency"    "iron deficiency"    NA                  
#> [17443] NA                   "no iron deficiency" "no iron deficiency"
#> [17446] "no iron deficiency" NA                   "no iron deficiency"
#> [17449] NA                   "no iron deficiency" "no iron deficiency"
#> [17452] NA                   "no iron deficiency" "no iron deficiency"
#> [17455] NA                   NA                   "no iron deficiency"
#> [17458] "no iron deficiency" "no iron deficiency" NA                  
#> [17461] NA                   "iron deficiency"    "no iron deficiency"
#> [17464] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17467] NA                   "no iron deficiency" "no iron deficiency"
#> [17470] NA                   NA                   NA                  
#> [17473] NA                   "no iron deficiency" "no iron deficiency"
#> [17476] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17479] "iron deficiency"    "no iron deficiency" NA                  
#> [17482] NA                   "no iron deficiency" "no iron deficiency"
#> [17485] "no iron deficiency" NA                   "no iron deficiency"
#> [17488] "no iron deficiency" NA                   "no iron deficiency"
#> [17491] "iron deficiency"    "no iron deficiency" NA                  
#> [17494] NA                   NA                   NA                  
#> [17497] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17500] NA                   "no iron deficiency" "no iron deficiency"
#> [17503] NA                   NA                   "iron deficiency"   
#> [17506] NA                   "iron deficiency"    NA                  
#> [17509] "no iron deficiency" "iron deficiency"    NA                  
#> [17512] NA                   NA                   NA                  
#> [17515] NA                   "no iron deficiency" "iron deficiency"   
#> [17518] NA                   NA                   "no iron deficiency"
#> [17521] "no iron deficiency" NA                   NA                  
#> [17524] "iron deficiency"    NA                   NA                  
#> [17527] NA                   NA                   NA                  
#> [17530] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17533] NA                   "no iron deficiency" NA                  
#> [17536] NA                   NA                   NA                  
#> [17539] "no iron deficiency" "iron deficiency"    NA                  
#> [17542] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17545] NA                   "no iron deficiency" NA                  
#> [17548] NA                   "iron deficiency"    "iron deficiency"   
#> [17551] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17554] "iron deficiency"    NA                   "no iron deficiency"
#> [17557] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17560] NA                   "iron deficiency"    "iron deficiency"   
#> [17563] "no iron deficiency" NA                   NA                  
#> [17566] "iron deficiency"    NA                   "no iron deficiency"
#> [17569] "no iron deficiency" NA                   "no iron deficiency"
#> [17572] "no iron deficiency" "no iron deficiency" NA                  
#> [17575] "iron deficiency"    NA                   NA                  
#> [17578] "iron deficiency"    NA                   "iron deficiency"   
#> [17581] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17584] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17587] "no iron deficiency" NA                   NA                  
#> [17590] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17593] "iron deficiency"    NA                   NA                  
#> [17596] "no iron deficiency" NA                   NA                  
#> [17599] "no iron deficiency" "no iron deficiency" NA                  
#> [17602] NA                   NA                   "no iron deficiency"
#> [17605] NA                   "iron deficiency"    NA                  
#> [17608] NA                   "iron deficiency"    NA                  
#> [17611] "no iron deficiency" NA                   "no iron deficiency"
#> [17614] "iron deficiency"    "iron deficiency"    NA                  
#> [17617] "iron deficiency"    "iron deficiency"    NA                  
#> [17620] NA                   NA                   NA                  
#> [17623] NA                   "iron deficiency"    "iron deficiency"   
#> [17626] NA                   NA                   NA                  
#> [17629] NA                   "iron deficiency"    NA                  
#> [17632] NA                   "iron deficiency"    "iron deficiency"   
#> [17635] NA                   "no iron deficiency" "no iron deficiency"
#> [17638] "iron deficiency"    NA                   NA                  
#> [17641] "no iron deficiency" NA                   NA                  
#> [17644] NA                   NA                   NA                  
#> [17647] NA                   "no iron deficiency" "no iron deficiency"
#> [17650] "no iron deficiency" NA                   NA                  
#> [17653] NA                   NA                   "no iron deficiency"
#> [17656] NA                   "no iron deficiency" NA                  
#> [17659] NA                   "iron deficiency"    NA                  
#> [17662] "no iron deficiency" NA                   "no iron deficiency"
#> [17665] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17668] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17671] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17674] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17677] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17680] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17683] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17686] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17689] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17692] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17695] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17698] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17701] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17704] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17707] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17710] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17713] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17716] NA                   "no iron deficiency" "iron deficiency"   
#> [17719] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17722] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17725] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17728] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17731] "no iron deficiency" "iron deficiency"    NA                  
#> [17734] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17737] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17740] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17743] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17746] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17749] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17752] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17755] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17758] "iron deficiency"    "no iron deficiency" NA                  
#> [17761] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17764] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17767] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17770] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17773] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17776] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17779] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17782] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17785] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17788] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17791] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17794] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17797] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17800] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17803] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17806] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17809] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17812] NA                   "no iron deficiency" "iron deficiency"   
#> [17815] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17818] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17821] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17824] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17827] "no iron deficiency" "iron deficiency"    NA                  
#> [17830] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17833] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17836] "no iron deficiency" NA                   "iron deficiency"   
#> [17839] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17842] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17845] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17848] NA                   "no iron deficiency" "no iron deficiency"
#> [17851] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17854] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17857] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17860] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17863] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17866] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17869] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17872] NA                   "no iron deficiency" "no iron deficiency"
#> [17875] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17878] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17881] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [17884] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17887] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17890] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17893] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [17896] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17899] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17902] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17905] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [17908] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17911] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17914] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17917] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17920] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17923] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17926] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17929] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17932] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17935] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17938] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17941] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17944] "no iron deficiency" NA                   "no iron deficiency"
#> [17947] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [17950] NA                   "no iron deficiency" "no iron deficiency"
#> [17953] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17956] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17959] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17962] "no iron deficiency" "no iron deficiency" NA                  
#> [17965] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17968] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17971] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [17974] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17977] "no iron deficiency" "iron deficiency"    NA                  
#> [17980] NA                   "no iron deficiency" "no iron deficiency"
#> [17983] "no iron deficiency" NA                   "no iron deficiency"
#> [17986] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [17989] "no iron deficiency" NA                   "iron deficiency"   
#> [17992] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [17995] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [17998] NA                   "iron deficiency"    "no iron deficiency"
#> [18001] "no iron deficiency" NA                   "no iron deficiency"
#> [18004] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18007] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18010] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18013] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18016] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18019] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18022] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18025] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18028] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18031] "no iron deficiency" NA                   NA                  
#> [18034] NA                   "no iron deficiency" "no iron deficiency"
#> [18037] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18040] "no iron deficiency" NA                   "no iron deficiency"
#> [18043] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18046] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18049] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18052] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18055] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18058] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18061] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18064] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18067] "no iron deficiency" "iron deficiency"    NA                  
#> [18070] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18073] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18076] "no iron deficiency" "no iron deficiency" NA                  
#> [18079] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18082] NA                   "no iron deficiency" "no iron deficiency"
#> [18085] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18088] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18091] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18094] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18097] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18100] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18103] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18106] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18109] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18112] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18115] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18118] "no iron deficiency" "no iron deficiency" NA                  
#> [18121] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18124] "iron deficiency"    NA                   "iron deficiency"   
#> [18127] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18130] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18133] "no iron deficiency" NA                   "iron deficiency"   
#> [18136] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18139] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18142] NA                   "iron deficiency"    NA                  
#> [18145] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18148] "iron deficiency"    NA                   NA                  
#> [18151] NA                   "no iron deficiency" "iron deficiency"   
#> [18154] "no iron deficiency" NA                   NA                  
#> [18157] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18160] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18163] NA                   "no iron deficiency" "iron deficiency"   
#> [18166] NA                   "no iron deficiency" "iron deficiency"   
#> [18169] NA                   "no iron deficiency" NA                  
#> [18172] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18175] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18178] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18181] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18184] "no iron deficiency" NA                   "no iron deficiency"
#> [18187] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18190] "iron deficiency"    "iron deficiency"    NA                  
#> [18193] NA                   "no iron deficiency" NA                  
#> [18196] "iron deficiency"    NA                   NA                  
#> [18199] "no iron deficiency" NA                   "no iron deficiency"
#> [18202] "iron deficiency"    NA                   "iron deficiency"   
#> [18205] "no iron deficiency" NA                   "no iron deficiency"
#> [18208] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18211] "no iron deficiency" "iron deficiency"    NA                  
#> [18214] NA                   "no iron deficiency" "no iron deficiency"
#> [18217] "iron deficiency"    NA                   "no iron deficiency"
#> [18220] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18223] "no iron deficiency" NA                   NA                  
#> [18226] "iron deficiency"    "iron deficiency"    NA                  
#> [18229] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18232] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18235] "no iron deficiency" NA                   "iron deficiency"   
#> [18238] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18241] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18244] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18247] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18250] "no iron deficiency" NA                   "iron deficiency"   
#> [18253] "iron deficiency"    NA                   "iron deficiency"   
#> [18256] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18259] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18262] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18265] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18268] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18271] "no iron deficiency" NA                   "no iron deficiency"
#> [18274] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18277] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18280] "no iron deficiency" NA                   "iron deficiency"   
#> [18283] "no iron deficiency" NA                   "iron deficiency"   
#> [18286] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18289] NA                   NA                   "iron deficiency"   
#> [18292] NA                   NA                   "no iron deficiency"
#> [18295] "no iron deficiency" "iron deficiency"    NA                  
#> [18298] NA                   "no iron deficiency" "no iron deficiency"
#> [18301] NA                   "no iron deficiency" "iron deficiency"   
#> [18304] "no iron deficiency" "no iron deficiency" NA                  
#> [18307] "no iron deficiency" NA                   NA                  
#> [18310] "no iron deficiency" "iron deficiency"    NA                  
#> [18313] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18316] NA                   NA                   NA                  
#> [18319] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18322] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18325] "no iron deficiency" NA                   "no iron deficiency"
#> [18328] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18331] NA                   "no iron deficiency" "no iron deficiency"
#> [18334] "iron deficiency"    NA                   "no iron deficiency"
#> [18337] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18340] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18343] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18346] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18349] "no iron deficiency" "iron deficiency"    NA                  
#> [18352] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18355] NA                   "no iron deficiency" "iron deficiency"   
#> [18358] "no iron deficiency" "iron deficiency"    NA                  
#> [18361] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18364] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18367] "iron deficiency"    "no iron deficiency" NA                  
#> [18370] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18373] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18376] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18379] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18382] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18385] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18388] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18391] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18394] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18397] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18400] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18403] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18406] NA                   "no iron deficiency" NA                  
#> [18409] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18412] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18415] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18418] "no iron deficiency" NA                   "iron deficiency"   
#> [18421] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18424] NA                   "no iron deficiency" "no iron deficiency"
#> [18427] NA                   NA                   "no iron deficiency"
#> [18430] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18433] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18436] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18439] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18442] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18445] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18448] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18451] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18454] "iron deficiency"    NA                   "no iron deficiency"
#> [18457] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18460] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18463] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18466] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18469] "no iron deficiency" "no iron deficiency" NA                  
#> [18472] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18475] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18478] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18481] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18484] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18487] "no iron deficiency" "no iron deficiency" NA                  
#> [18490] NA                   "no iron deficiency" "iron deficiency"   
#> [18493] NA                   NA                   NA                  
#> [18496] "no iron deficiency" NA                   "no iron deficiency"
#> [18499] NA                   NA                   "no iron deficiency"
#> [18502] "no iron deficiency" NA                   NA                  
#> [18505] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18508] "iron deficiency"    NA                   "no iron deficiency"
#> [18511] "iron deficiency"    NA                   "iron deficiency"   
#> [18514] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18517] NA                   "no iron deficiency" NA                  
#> [18520] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18523] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18526] NA                   NA                   "no iron deficiency"
#> [18529] NA                   "iron deficiency"    "iron deficiency"   
#> [18532] "no iron deficiency" NA                   "no iron deficiency"
#> [18535] "iron deficiency"    NA                   "iron deficiency"   
#> [18538] "no iron deficiency" "no iron deficiency" NA                  
#> [18541] "no iron deficiency" NA                   "iron deficiency"   
#> [18544] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18547] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18550] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18553] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18556] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18559] NA                   NA                   "no iron deficiency"
#> [18562] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18565] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18568] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18571] "no iron deficiency" "no iron deficiency" NA                  
#> [18574] "iron deficiency"    "no iron deficiency" NA                  
#> [18577] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18580] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18583] "no iron deficiency" NA                   NA                  
#> [18586] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18589] "no iron deficiency" NA                   "no iron deficiency"
#> [18592] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18595] "no iron deficiency" NA                   NA                  
#> [18598] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18601] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18604] "no iron deficiency" NA                   "no iron deficiency"
#> [18607] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18610] "no iron deficiency" NA                   "iron deficiency"   
#> [18613] "iron deficiency"    NA                   "iron deficiency"   
#> [18616] NA                   "no iron deficiency" "no iron deficiency"
#> [18619] "iron deficiency"    NA                   "no iron deficiency"
#> [18622] NA                   "iron deficiency"    "iron deficiency"   
#> [18625] NA                   "iron deficiency"    "no iron deficiency"
#> [18628] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18631] "no iron deficiency" "iron deficiency"    NA                  
#> [18634] NA                   "no iron deficiency" "no iron deficiency"
#> [18637] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18640] NA                   "no iron deficiency" NA                  
#> [18643] NA                   NA                   "iron deficiency"   
#> [18646] NA                   "iron deficiency"    NA                  
#> [18649] "no iron deficiency" "iron deficiency"    NA                  
#> [18652] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18655] "iron deficiency"    "no iron deficiency" NA                  
#> [18658] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18661] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18664] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18667] "no iron deficiency" NA                   NA                  
#> [18670] NA                   "iron deficiency"    "no iron deficiency"
#> [18673] "no iron deficiency" NA                   "no iron deficiency"
#> [18676] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18679] NA                   "no iron deficiency" "iron deficiency"   
#> [18682] NA                   NA                   NA                  
#> [18685] "iron deficiency"    NA                   "iron deficiency"   
#> [18688] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18691] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18694] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18697] "iron deficiency"    "no iron deficiency" NA                  
#> [18700] NA                   "no iron deficiency" "iron deficiency"   
#> [18703] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18706] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18709] "no iron deficiency" "iron deficiency"    NA                  
#> [18712] NA                   "iron deficiency"    "iron deficiency"   
#> [18715] NA                   "no iron deficiency" NA                  
#> [18718] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18721] "iron deficiency"    NA                   "iron deficiency"   
#> [18724] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18727] "iron deficiency"    NA                   "iron deficiency"   
#> [18730] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18733] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18736] "no iron deficiency" "no iron deficiency" NA                  
#> [18739] "no iron deficiency" NA                   NA                  
#> [18742] "no iron deficiency" "iron deficiency"    NA                  
#> [18745] NA                   "no iron deficiency" "iron deficiency"   
#> [18748] NA                   "no iron deficiency" "no iron deficiency"
#> [18751] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18754] "no iron deficiency" "no iron deficiency" NA                  
#> [18757] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18760] NA                   NA                   "no iron deficiency"
#> [18763] NA                   NA                   "no iron deficiency"
#> [18766] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18769] NA                   "no iron deficiency" "iron deficiency"   
#> [18772] NA                   "iron deficiency"    "no iron deficiency"
#> [18775] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18778] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18781] NA                   "iron deficiency"    "iron deficiency"   
#> [18784] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18787] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18790] NA                   "no iron deficiency" "no iron deficiency"
#> [18793] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18796] NA                   "iron deficiency"    "iron deficiency"   
#> [18799] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18802] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18805] NA                   "no iron deficiency" "iron deficiency"   
#> [18808] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18811] NA                   NA                   "no iron deficiency"
#> [18814] NA                   "iron deficiency"    NA                  
#> [18817] NA                   NA                   "no iron deficiency"
#> [18820] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18823] NA                   "no iron deficiency" "iron deficiency"   
#> [18826] "iron deficiency"    NA                   "iron deficiency"   
#> [18829] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18832] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18835] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18838] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18841] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18844] "iron deficiency"    NA                   "iron deficiency"   
#> [18847] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18850] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18853] "iron deficiency"    NA                   "no iron deficiency"
#> [18856] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18859] NA                   "no iron deficiency" "iron deficiency"   
#> [18862] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18865] NA                   "iron deficiency"    "no iron deficiency"
#> [18868] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [18871] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18874] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18877] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18880] NA                   NA                   "no iron deficiency"
#> [18883] "iron deficiency"    NA                   "iron deficiency"   
#> [18886] "iron deficiency"    "iron deficiency"    NA                  
#> [18889] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18892] "no iron deficiency" "no iron deficiency" NA                  
#> [18895] NA                   "no iron deficiency" "iron deficiency"   
#> [18898] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18901] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18904] "no iron deficiency" "iron deficiency"    NA                  
#> [18907] "iron deficiency"    NA                   "no iron deficiency"
#> [18910] "iron deficiency"    "no iron deficiency" NA                  
#> [18913] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18916] NA                   "no iron deficiency" NA                  
#> [18919] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18922] NA                   NA                   NA                  
#> [18925] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18928] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [18931] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [18934] "iron deficiency"    NA                   NA                  
#> [18937] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18940] "no iron deficiency" NA                   "no iron deficiency"
#> [18943] "no iron deficiency" "no iron deficiency" NA                  
#> [18946] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18949] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18952] "no iron deficiency" "no iron deficiency" NA                  
#> [18955] "no iron deficiency" "no iron deficiency" NA                  
#> [18958] NA                   NA                   "no iron deficiency"
#> [18961] "no iron deficiency" NA                   "no iron deficiency"
#> [18964] NA                   "iron deficiency"    "iron deficiency"   
#> [18967] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18970] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [18973] "iron deficiency"    NA                   "iron deficiency"   
#> [18976] "no iron deficiency" "no iron deficiency" NA                  
#> [18979] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [18982] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [18985] "no iron deficiency" NA                   "iron deficiency"   
#> [18988] "no iron deficiency" NA                   "no iron deficiency"
#> [18991] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [18994] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [18997] "no iron deficiency" NA                   "no iron deficiency"
#> [19000] "no iron deficiency" NA                   "iron deficiency"   
#> [19003] "iron deficiency"    "no iron deficiency" NA                  
#> [19006] "no iron deficiency" NA                   "no iron deficiency"
#> [19009] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19012] "no iron deficiency" "iron deficiency"    NA                  
#> [19015] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19018] NA                   NA                   NA                  
#> [19021] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19024] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19027] "no iron deficiency" "no iron deficiency" NA                  
#> [19030] "iron deficiency"    "iron deficiency"    NA                  
#> [19033] NA                   "no iron deficiency" "no iron deficiency"
#> [19036] "no iron deficiency" NA                   "no iron deficiency"
#> [19039] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19042] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19045] NA                   "no iron deficiency" "no iron deficiency"
#> [19048] "iron deficiency"    "no iron deficiency" NA                  
#> [19051] NA                   NA                   NA                  
#> [19054] NA                   "no iron deficiency" "iron deficiency"   
#> [19057] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19060] NA                   NA                   "no iron deficiency"
#> [19063] "iron deficiency"    NA                   NA                  
#> [19066] NA                   "no iron deficiency" "iron deficiency"   
#> [19069] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19072] "no iron deficiency" "iron deficiency"    NA                  
#> [19075] "no iron deficiency" NA                   "no iron deficiency"
#> [19078] NA                   "iron deficiency"    NA                  
#> [19081] NA                   "iron deficiency"    NA                  
#> [19084] NA                   "no iron deficiency" "iron deficiency"   
#> [19087] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19090] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19093] "iron deficiency"    NA                   "iron deficiency"   
#> [19096] NA                   "iron deficiency"    "no iron deficiency"
#> [19099] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19102] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19105] NA                   "iron deficiency"    NA                  
#> [19108] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19111] NA                   "no iron deficiency" NA                  
#> [19114] "no iron deficiency" NA                   "no iron deficiency"
#> [19117] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19120] "no iron deficiency" "iron deficiency"    NA                  
#> [19123] "iron deficiency"    NA                   NA                  
#> [19126] NA                   "no iron deficiency" NA                  
#> [19129] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19132] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19135] NA                   NA                   NA                  
#> [19138] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19141] NA                   "iron deficiency"    "iron deficiency"   
#> [19144] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19147] "iron deficiency"    NA                   "no iron deficiency"
#> [19150] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19153] "no iron deficiency" "iron deficiency"    NA                  
#> [19156] NA                   "no iron deficiency" "iron deficiency"   
#> [19159] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19162] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19165] NA                   "iron deficiency"    "iron deficiency"   
#> [19168] NA                   "iron deficiency"    "no iron deficiency"
#> [19171] "iron deficiency"    "iron deficiency"    NA                  
#> [19174] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19177] NA                   "no iron deficiency" "iron deficiency"   
#> [19180] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19183] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19186] NA                   "no iron deficiency" "no iron deficiency"
#> [19189] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19192] "no iron deficiency" "iron deficiency"    NA                  
#> [19195] NA                   "no iron deficiency" "iron deficiency"   
#> [19198] "iron deficiency"    "no iron deficiency" NA                  
#> [19201] NA                   "iron deficiency"    "no iron deficiency"
#> [19204] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19207] "iron deficiency"    NA                   NA                  
#> [19210] NA                   "no iron deficiency" "no iron deficiency"
#> [19213] "no iron deficiency" NA                   "iron deficiency"   
#> [19216] NA                   "iron deficiency"    "no iron deficiency"
#> [19219] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19222] NA                   "iron deficiency"    "no iron deficiency"
#> [19225] NA                   NA                   "no iron deficiency"
#> [19228] "iron deficiency"    NA                   "iron deficiency"   
#> [19231] "no iron deficiency" NA                   "no iron deficiency"
#> [19234] NA                   "iron deficiency"    "iron deficiency"   
#> [19237] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19240] NA                   NA                   "iron deficiency"   
#> [19243] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19246] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19249] NA                   NA                   "iron deficiency"   
#> [19252] "iron deficiency"    NA                   "no iron deficiency"
#> [19255] NA                   "iron deficiency"    NA                  
#> [19258] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19261] NA                   "no iron deficiency" "iron deficiency"   
#> [19264] "iron deficiency"    "iron deficiency"    NA                  
#> [19267] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19270] "iron deficiency"    NA                   "no iron deficiency"
#> [19273] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19276] "no iron deficiency" NA                   "no iron deficiency"
#> [19279] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19282] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19285] NA                   "no iron deficiency" "no iron deficiency"
#> [19288] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19291] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [19294] "no iron deficiency" "no iron deficiency" NA                  
#> [19297] "iron deficiency"    "no iron deficiency" NA                  
#> [19300] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19303] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19306] NA                   "no iron deficiency" "iron deficiency"   
#> [19309] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19312] "iron deficiency"    NA                   "no iron deficiency"
#> [19315] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19318] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19321] NA                   "no iron deficiency" "iron deficiency"   
#> [19324] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19327] "no iron deficiency" "iron deficiency"    NA                  
#> [19330] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19333] "no iron deficiency" "iron deficiency"    NA                  
#> [19336] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19339] "iron deficiency"    "no iron deficiency" NA                  
#> [19342] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19345] "iron deficiency"    NA                   NA                  
#> [19348] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19351] "no iron deficiency" NA                   NA                  
#> [19354] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19357] "no iron deficiency" NA                   NA                  
#> [19360] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19363] "no iron deficiency" NA                   NA                  
#> [19366] "iron deficiency"    NA                   "iron deficiency"   
#> [19369] "iron deficiency"    NA                   "no iron deficiency"
#> [19372] "iron deficiency"    NA                   NA                  
#> [19375] NA                   NA                   "no iron deficiency"
#> [19378] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19381] "no iron deficiency" NA                   "no iron deficiency"
#> [19384] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19387] "iron deficiency"    NA                   "no iron deficiency"
#> [19390] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19393] "iron deficiency"    "no iron deficiency" "no iron deficiency"
#> [19396] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19399] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19402] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19405] "iron deficiency"    NA                   "iron deficiency"   
#> [19408] "no iron deficiency" "iron deficiency"    "iron deficiency"   
#> [19411] "iron deficiency"    "iron deficiency"    "iron deficiency"   
#> [19414] "no iron deficiency" "no iron deficiency" "no iron deficiency"
#> [19417] "iron deficiency"    "no iron deficiency" NA                  
#> [19420] NA                   "no iron deficiency" "no iron deficiency"
#> [19423] "no iron deficiency" "no iron deficiency" "iron deficiency"   
#> [19426] "no iron deficiency" NA                   "iron deficiency"   
#> [19429] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19432] NA                   NA                   "iron deficiency"   
#> [19435] "iron deficiency"    "iron deficiency"    "no iron deficiency"
#> [19438] "iron deficiency"    "no iron deficiency" "iron deficiency"   
#> [19441] "no iron deficiency" "iron deficiency"    "no iron deficiency"
#> [19444] NA                   "no iron deficiency" NA                  
#> [19447] "iron deficiency"    NA                   NA                  

 # Iron storage status based on AGP only
 ferritin_corrected <- correct_ferritin(
   agp = 2, ferritin = mnData$ferritin[1]
 )
 detect_iron_deficiency(ferritin_corrected)
#> [1] "iron deficiency"

 # Iron storage status based on CRP and AGP
 ferritin_corrected <- correct_ferritin(
   crp = mnData$crp[1], agp = 2, ferritin = mnData$ferritin[1]
 )
 detect_iron_deficiency(ferritin_corrected)
#> [1] "iron deficiency"

 # Iron storage status - qualitative
 detect_iron_deficiency_qualitative(
   ferritin = 3, inflammation = TRUE
 )
#> [1] "iron deficiency"
 detect_iron_deficiency_qualitative(
   ferritin = c(2, 3, 5), inflammation = c(TRUE, FALSE, TRUE)
 )
#> [1] "iron deficiency" "iron deficiency" "iron deficiency"
```
