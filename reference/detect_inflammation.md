# Determine inflammation status

Given laboratory values for serum c-reactive protein (CRP) and/or serum
alpha(1)-acid-glycoprotein (AGP), the inflammation status of a subject
can be determined based on cut-off values described in Namaste, S. M.,
Rohner, F., Huang, J., Bhushan, N. L., Flores-Ayala, R., Kupka, R., Mei,
Z., Rawat, R., Williams, A. M., Raiten, D. J., Northrop-Clewes, C. A., &
Suchdev, P. S. (2017). Adjusting ferritin concentrations for
inflammation: Biomarkers Reflecting Inflammation and Nutritional
Determinants of Anemia (BRINDA) project. The American journal of
clinical nutrition, 106(Suppl 1), 359S–371S.
https://doi.org/10.3945/ajcn.116.141762

## Usage

``` r
detect_inflammation(crp = NULL, agp = NULL, label = TRUE)

detect_inflammation_crp(crp = NULL, label = TRUE)

detect_inflammation_agp(agp = NULL, label = TRUE)
```

## Arguments

- crp:

  A numeric value or numeric vector of c-reactive protein (crp) values
  in micrograms per litre (microgram/l).

- agp:

  A numeric value or numeric vector of alpha(1)-acid-glycoprotein (agp)
  values in micrograms per litre (microgram/l).

- label:

  Logical. Should labels be used to classify inflammation status? If
  TRUE (default), status is classified as "no inflammation" or
  "inflammation" based on either CRP or AGP or status is classified as
  "no inflammation", "incubation", "early convalescence", or "late
  convalescence" based on both CRP and AGP. If FALSE, simple integer
  codes are returned: 0 for no inflammation and 1 for inflammation based
  on either CRP or AGP; 0 for no inflammation, 1 for incubation, 2 for
  early convalescence, or 3 for late convalescence.

## Value

If `label` is TRUE, a character value or character vector of
inflammation classification based on c-reactive protein (CRP) and/or
alpha(1)-acid-glycoprotein (AGP) values. If `label` is FALSE, an integer
value or vector of inflammation classification.

## Author

Nicholus Tint Zaw and Ernest Guevarra

## Examples

``` r
## Detect inflammation by AGP
detect_inflammation_agp(2)
#> [1] "inflammation"
detect_inflammation_agp(2, label = FALSE)
#> [1] 1

## Detect inflammation by CRP
detect_inflammation_crp(2)
#> [1] "no inflammation"
detect_inflammation(crp = mnData$crp)
#>     [1] "no inflammation" "inflammation"    NA                "no inflammation"
#>     [5] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>     [9] NA                NA                "inflammation"    NA               
#>    [13] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>    [17] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>    [21] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>    [25] NA                "no inflammation" NA                "no inflammation"
#>    [29] NA                NA                "inflammation"    "inflammation"   
#>    [33] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>    [37] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>    [41] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>    [45] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>    [49] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>    [53] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>    [57] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>    [61] NA                "inflammation"    "inflammation"    "inflammation"   
#>    [65] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>    [69] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>    [73] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>    [77] "inflammation"    "inflammation"    "inflammation"    NA               
#>    [81] "inflammation"    "inflammation"    NA                "no inflammation"
#>    [85] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>    [89] NA                NA                "inflammation"    "no inflammation"
#>    [93] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>    [97] NA                NA                "inflammation"    "no inflammation"
#>   [101] NA                "inflammation"    "inflammation"    "inflammation"   
#>   [105] "no inflammation" NA                "inflammation"    "inflammation"   
#>   [109] "inflammation"    "inflammation"    NA                "no inflammation"
#>   [113] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>   [117] NA                "no inflammation" "inflammation"    "no inflammation"
#>   [121] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [125] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [129] NA                "inflammation"    NA                "inflammation"   
#>   [133] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [137] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [141] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [145] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>   [149] "inflammation"    "inflammation"    NA                "inflammation"   
#>   [153] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [157] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>   [161] NA                "no inflammation" "inflammation"    "inflammation"   
#>   [165] "no inflammation" "inflammation"    "inflammation"    NA               
#>   [169] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [173] "inflammation"    "inflammation"    NA                NA               
#>   [177] "inflammation"    "inflammation"    NA                "no inflammation"
#>   [181] "no inflammation" "inflammation"    "no inflammation" NA               
#>   [185] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [189] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>   [193] "no inflammation" NA                "no inflammation" "no inflammation"
#>   [197] NA                NA                "inflammation"    NA               
#>   [201] "no inflammation" "inflammation"    NA                NA               
#>   [205] NA                NA                NA                NA               
#>   [209] "inflammation"    NA                NA                "no inflammation"
#>   [213] "no inflammation" "no inflammation" "inflammation"    NA               
#>   [217] NA                "no inflammation" NA                NA               
#>   [221] NA                NA                NA                "no inflammation"
#>   [225] NA                NA                NA                "inflammation"   
#>   [229] "inflammation"    "inflammation"    NA                "inflammation"   
#>   [233] NA                NA                "inflammation"    "inflammation"   
#>   [237] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [241] "no inflammation" "no inflammation" NA                "inflammation"   
#>   [245] NA                "no inflammation" "inflammation"    NA               
#>   [249] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [253] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [257] "inflammation"    "inflammation"    "no inflammation" NA               
#>   [261] "inflammation"    NA                "inflammation"    "no inflammation"
#>   [265] NA                "inflammation"    "no inflammation" "inflammation"   
#>   [269] NA                "inflammation"    "inflammation"    "inflammation"   
#>   [273] "inflammation"    NA                "inflammation"    "inflammation"   
#>   [277] "inflammation"    NA                "inflammation"    "inflammation"   
#>   [281] NA                "inflammation"    "no inflammation" "no inflammation"
#>   [285] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>   [289] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [293] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [297] NA                "no inflammation" "inflammation"    "no inflammation"
#>   [301] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>   [305] "inflammation"    "no inflammation" NA                "no inflammation"
#>   [309] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>   [313] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [317] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [321] "no inflammation" "inflammation"    NA                NA               
#>   [325] "inflammation"    "no inflammation" NA                "no inflammation"
#>   [329] "no inflammation" "inflammation"    "no inflammation" NA               
#>   [333] "inflammation"    "inflammation"    NA                "no inflammation"
#>   [337] "inflammation"    NA                NA                "inflammation"   
#>   [341] "inflammation"    NA                "no inflammation" "inflammation"   
#>   [345] NA                "inflammation"    NA                "inflammation"   
#>   [349] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [353] "no inflammation" "inflammation"    NA                NA               
#>   [357] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>   [361] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [365] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [369] "inflammation"    NA                "inflammation"    "inflammation"   
#>   [373] "no inflammation" "inflammation"    "no inflammation" NA               
#>   [377] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>   [381] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [385] "no inflammation" NA                "no inflammation" "no inflammation"
#>   [389] "inflammation"    NA                "inflammation"    "inflammation"   
#>   [393] NA                "inflammation"    "inflammation"    "inflammation"   
#>   [397] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [401] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>   [405] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [409] NA                "inflammation"    "inflammation"    "inflammation"   
#>   [413] "no inflammation" "no inflammation" NA                "inflammation"   
#>   [417] NA                "inflammation"    NA                "no inflammation"
#>   [421] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [425] "no inflammation" "inflammation"    "no inflammation" NA               
#>   [429] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [433] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>   [437] "inflammation"    "inflammation"    NA                "no inflammation"
#>   [441] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>   [445] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [449] NA                "no inflammation" "no inflammation" "inflammation"   
#>   [453] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [457] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [461] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [465] "no inflammation" "no inflammation" "inflammation"    NA               
#>   [469] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [473] "inflammation"    "no inflammation" "no inflammation" NA               
#>   [477] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [481] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [485] "no inflammation" NA                "no inflammation" "no inflammation"
#>   [489] "inflammation"    "inflammation"    NA                "inflammation"   
#>   [493] "inflammation"    "no inflammation" NA                "no inflammation"
#>   [497] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>   [501] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [505] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [509] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [513] "inflammation"    NA                "no inflammation" "no inflammation"
#>   [517] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [521] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>   [525] "no inflammation" "no inflammation" NA                "inflammation"   
#>   [529] NA                NA                "no inflammation" "no inflammation"
#>   [533] "inflammation"    "no inflammation" "no inflammation" NA               
#>   [537] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [541] "inflammation"    "inflammation"    "inflammation"    NA               
#>   [545] "inflammation"    "no inflammation" "no inflammation" NA               
#>   [549] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [553] "no inflammation" NA                "inflammation"    "no inflammation"
#>   [557] "no inflammation" NA                "inflammation"    "inflammation"   
#>   [561] "no inflammation" NA                "no inflammation" "no inflammation"
#>   [565] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [569] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [573] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [577] "inflammation"    "inflammation"    NA                NA               
#>   [581] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [585] "no inflammation" "inflammation"    NA                NA               
#>   [589] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [593] NA                "inflammation"    "no inflammation" NA               
#>   [597] "inflammation"    NA                NA                NA               
#>   [601] "inflammation"    NA                NA                NA               
#>   [605] NA                NA                NA                NA               
#>   [609] NA                "inflammation"    "no inflammation" "no inflammation"
#>   [613] NA                "no inflammation" "inflammation"    "inflammation"   
#>   [617] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [621] "inflammation"    NA                NA                "inflammation"   
#>   [625] NA                "no inflammation" "no inflammation" "inflammation"   
#>   [629] "no inflammation" "inflammation"    NA                NA               
#>   [633] "inflammation"    "no inflammation" NA                "no inflammation"
#>   [637] "inflammation"    "inflammation"    NA                "inflammation"   
#>   [641] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [645] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [649] NA                "no inflammation" "inflammation"    "no inflammation"
#>   [653] "no inflammation" NA                NA                "no inflammation"
#>   [657] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [661] NA                "no inflammation" "inflammation"    NA               
#>   [665] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [669] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [673] "no inflammation" "no inflammation" "inflammation"    NA               
#>   [677] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [681] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>   [685] "inflammation"    "inflammation"    NA                "no inflammation"
#>   [689] "no inflammation" NA                NA                NA               
#>   [693] "inflammation"    "no inflammation" NA                "inflammation"   
#>   [697] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [701] "inflammation"    NA                "no inflammation" "inflammation"   
#>   [705] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>   [709] "inflammation"    NA                NA                NA               
#>   [713] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [717] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [721] "no inflammation" NA                "no inflammation" "no inflammation"
#>   [725] NA                "no inflammation" "no inflammation" "inflammation"   
#>   [729] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [733] "no inflammation" NA                "inflammation"    "no inflammation"
#>   [737] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [741] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [745] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [749] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [753] "no inflammation" NA                "inflammation"    "inflammation"   
#>   [757] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [761] "inflammation"    "no inflammation" NA                NA               
#>   [765] "inflammation"    "no inflammation" "inflammation"    NA               
#>   [769] "no inflammation" "no inflammation" "inflammation"    NA               
#>   [773] NA                "inflammation"    "inflammation"    "inflammation"   
#>   [777] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [781] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [785] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [789] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [793] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [797] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [801] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [805] "no inflammation" "no inflammation" NA                "no inflammation"
#>   [809] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>   [813] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [817] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [821] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [825] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [829] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [833] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>   [837] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [841] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [845] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [849] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [853] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [857] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [861] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [865] "no inflammation" NA                "no inflammation" "inflammation"   
#>   [869] "inflammation"    "no inflammation" "no inflammation" NA               
#>   [873] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [877] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [881] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [885] "inflammation"    NA                "no inflammation" "no inflammation"
#>   [889] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [893] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>   [897] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [901] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [905] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [909] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [913] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [917] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [921] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [925] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>   [929] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [933] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [937] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>   [941] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [945] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [949] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [953] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [957] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>   [961] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>   [965] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [969] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [973] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>   [977] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>   [981] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>   [985] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>   [989] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>   [993] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>   [997] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1001] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [1005] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1009] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1013] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [1017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1021] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1025] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1029] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1033] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1037] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1041] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1045] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1049] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1053] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1057] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1061] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1065] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1069] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1073] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1077] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [1081] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1085] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [1089] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1093] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1097] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1101] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1105] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1109] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1113] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1117] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1121] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1125] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1129] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1133] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1137] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1141] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1145] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1149] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [1153] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1157] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1161] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1165] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1169] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [1173] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1177] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1181] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1185] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1189] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [1193] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1197] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1201] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1205] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1209] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1213] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1217] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1221] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1225] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1229] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1233] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1237] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [1241] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1245] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1249] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1253] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1257] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1261] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1265] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1269] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [1273] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1277] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1281] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1285] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1289] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1293] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1297] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1301] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1305] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1309] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1313] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1317] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1321] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1325] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [1329] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1333] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1337] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1341] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1345] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1349] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1353] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1357] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [1361] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1365] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1369] NA                NA                "no inflammation" "inflammation"   
#>  [1373] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1377] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1381] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [1385] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [1389] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1393] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1397] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [1401] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1405] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1409] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1413] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1417] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1421] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1425] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1429] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1433] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1437] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1441] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1445] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1449] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1453] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1457] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1461] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1465] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [1469] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1473] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1477] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1481] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1485] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1489] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1493] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1497] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1501] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1505] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [1509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1513] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1517] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1521] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1525] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1529] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [1533] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1537] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1541] NA                "inflammation"    "inflammation"    NA               
#>  [1545] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1549] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1553] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [1557] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1561] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1565] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [1569] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1577] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [1581] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [1585] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1589] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1593] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1597] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1601] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1605] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1609] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [1613] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1617] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1621] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1625] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1629] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1633] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1637] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1641] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1645] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [1649] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1653] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1657] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1661] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1665] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1669] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [1673] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1677] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1681] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1685] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1689] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1693] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1697] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1701] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1705] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1709] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1713] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1717] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1721] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1725] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [1729] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1733] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1737] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1741] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1745] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1749] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1753] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1757] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1761] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [1765] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1769] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1773] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1777] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [1781] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1785] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1789] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1793] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1797] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1801] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1805] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1809] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1817] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [1821] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [1825] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1829] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1833] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1837] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1841] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [1845] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [1849] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1853] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [1857] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1861] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [1865] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [1869] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [1873] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1877] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [1881] "inflammation"    NA                "no inflammation" NA               
#>  [1885] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1889] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [1893] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1897] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [1901] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1905] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [1909] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [1913] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [1917] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [1921] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [1925] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [1929] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1933] NA                NA                "no inflammation" "no inflammation"
#>  [1937] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [1941] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [1945] NA                "no inflammation" NA                "no inflammation"
#>  [1949] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [1953] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [1957] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [1961] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [1965] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1969] NA                NA                "no inflammation" "inflammation"   
#>  [1973] "no inflammation" "no inflammation" NA                NA               
#>  [1977] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [1981] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [1985] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [1989] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [1993] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [1997] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [2001] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2005] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [2009] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [2013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2021] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2025] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2029] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [2033] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2037] NA                NA                "no inflammation" "inflammation"   
#>  [2041] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [2045] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2049] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2053] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2057] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2061] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [2065] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2069] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [2073] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2077] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2081] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2085] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2089] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2093] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2097] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [2101] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2105] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2109] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2113] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2117] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2121] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2125] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2129] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2133] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2137] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2141] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2145] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2149] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [2153] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [2157] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2161] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2165] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [2169] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [2173] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [2177] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2181] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2185] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2189] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [2193] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2197] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2201] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [2205] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [2209] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2213] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [2217] "no inflammation" NA                NA                "no inflammation"
#>  [2221] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2225] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2229] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [2233] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2237] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2241] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2245] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2249] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2253] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2257] "no inflammation" NA                NA                "no inflammation"
#>  [2261] NA                NA                "no inflammation" "inflammation"   
#>  [2265] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2269] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2273] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2277] NA                "inflammation"    NA                "inflammation"   
#>  [2281] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2285] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [2289] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [2293] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [2297] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2301] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [2305] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2309] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2313] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [2317] "inflammation"    NA                "inflammation"    NA               
#>  [2321] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [2325] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [2329] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [2333] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2337] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [2341] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2345] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [2349] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2353] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2357] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [2361] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2365] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [2369] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [2373] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [2377] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2381] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [2385] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [2389] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [2393] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [2397] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [2401] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [2405] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [2409] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2413] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2417] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2421] NA                NA                "no inflammation" "no inflammation"
#>  [2425] "no inflammation" "no inflammation" NA                NA               
#>  [2429] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2433] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2437] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2441] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2445] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2449] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2453] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2457] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [2461] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2465] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2469] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [2473] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2477] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2481] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [2485] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [2489] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [2493] "no inflammation" "no inflammation" NA                NA               
#>  [2497] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2501] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [2505] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2509] NA                NA                NA                "no inflammation"
#>  [2513] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2517] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [2521] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2525] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2529] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [2533] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [2537] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2541] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2545] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [2549] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2553] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2557] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2561] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2565] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2569] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [2573] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2577] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2581] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2585] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2589] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2593] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [2597] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [2601] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2605] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [2609] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2613] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2617] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2621] "no inflammation" NA                NA                "no inflammation"
#>  [2625] "no inflammation" "inflammation"    NA                NA               
#>  [2629] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2633] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [2637] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2641] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [2645] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2649] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2653] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2657] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2661] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2665] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2669] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2673] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [2677] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2681] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2685] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2689] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [2693] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2697] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2701] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2705] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2709] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [2713] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2717] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2721] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [2725] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2729] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2733] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [2737] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2741] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2745] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2749] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2753] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2757] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [2761] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2765] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2769] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [2773] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [2777] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2781] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2785] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2789] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2793] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2797] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2801] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2805] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2809] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [2813] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2817] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2821] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [2825] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [2829] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2833] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2837] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2841] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2845] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [2849] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [2853] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [2857] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [2861] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2865] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [2869] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2873] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [2877] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [2881] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2885] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [2889] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2893] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [2897] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2901] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [2905] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [2909] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2913] "inflammation"    NA                NA                "inflammation"   
#>  [2917] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [2921] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [2925] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2929] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [2933] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [2937] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2941] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2945] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [2949] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [2953] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2957] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2961] NA                "no inflammation" NA                "no inflammation"
#>  [2965] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [2969] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [2973] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [2977] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [2981] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [2985] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [2989] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [2993] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [2997] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3001] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3005] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3009] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3017] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3021] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3025] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [3029] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3033] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3037] NA                NA                "inflammation"    "inflammation"   
#>  [3041] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3045] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3049] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3053] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [3057] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3061] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [3065] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [3069] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3073] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3077] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3081] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [3085] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3089] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3093] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [3097] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3101] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3105] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3109] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [3113] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3117] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3121] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3125] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3129] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3133] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3137] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3141] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3145] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3149] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3153] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3157] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3161] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3165] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [3169] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [3173] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [3177] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3181] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3185] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3189] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3193] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3197] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3201] "no inflammation" NA                NA                "inflammation"   
#>  [3205] "no inflammation" "inflammation"    NA                NA               
#>  [3209] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3213] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3217] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3221] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [3225] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3229] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3233] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3237] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3241] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3245] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3249] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3253] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [3257] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3261] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3265] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3269] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3273] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3277] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3281] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3285] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3289] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3293] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3297] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3301] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3305] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3309] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3313] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3317] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3321] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [3325] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3329] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [3333] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3337] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3341] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [3345] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3349] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3353] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3357] NA                "no inflammation" NA                "no inflammation"
#>  [3361] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3365] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3369] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3373] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3377] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3381] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3385] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3389] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3393] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3397] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3401] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [3405] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3409] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [3413] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [3417] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [3421] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3425] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3429] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3433] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3437] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3441] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3445] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [3449] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3453] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3457] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3461] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [3465] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3469] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3473] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3477] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3481] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [3485] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3489] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3493] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3497] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [3501] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3505] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3509] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [3513] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3517] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3521] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3525] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3529] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3533] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [3537] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [3541] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3545] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [3549] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3553] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3557] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3561] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3565] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3569] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3573] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3577] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3581] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3585] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3589] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [3593] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [3597] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3601] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3605] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3609] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3613] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [3617] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3621] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3625] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3629] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3633] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3637] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3641] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3645] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3649] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [3653] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3657] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3661] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [3665] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3669] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3673] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [3677] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [3681] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3685] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3689] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [3693] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [3697] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [3701] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3705] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3709] "inflammation"    NA                NA                "no inflammation"
#>  [3713] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3717] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3721] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3725] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3729] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [3733] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [3737] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3741] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3745] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3749] "no inflammation" "inflammation"    NA                NA               
#>  [3753] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [3757] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3761] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3765] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [3769] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3773] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3777] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [3781] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [3785] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3789] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3793] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3797] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3801] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3805] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3809] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3813] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3817] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3821] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [3825] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3829] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3833] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3837] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [3841] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [3845] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3849] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3853] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3857] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3861] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3865] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3869] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3873] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3877] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3881] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [3885] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [3889] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [3893] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3897] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3901] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [3905] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3909] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [3913] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3917] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [3921] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [3925] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3929] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [3933] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [3937] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [3941] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [3945] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [3949] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [3953] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [3957] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3961] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3965] "inflammation"    "no inflammation" NA                NA               
#>  [3969] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3973] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [3977] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [3981] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [3985] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [3989] "inflammation"    NA                "no inflammation" NA               
#>  [3993] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [3997] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4001] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4005] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4009] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [4013] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4017] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [4021] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4025] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [4029] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4033] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4037] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [4041] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [4045] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4049] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4053] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4057] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [4061] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4065] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4069] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4073] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4077] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4081] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [4085] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4089] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [4093] NA                "inflammation"    NA                "no inflammation"
#>  [4097] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4101] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [4105] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4109] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4113] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4117] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4121] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [4125] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [4129] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [4133] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4137] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [4141] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [4145] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4149] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4153] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4157] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [4161] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4165] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4169] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4173] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4177] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4181] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4185] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [4189] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4193] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4197] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [4201] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4205] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [4209] NA                NA                "inflammation"    "inflammation"   
#>  [4213] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [4217] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4221] "no inflammation" NA                "inflammation"    NA               
#>  [4225] "inflammation"    NA                "inflammation"    NA               
#>  [4229] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4233] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [4237] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4241] "inflammation"    NA                NA                NA               
#>  [4245] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [4249] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4253] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4257] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [4261] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4265] NA                "no inflammation" NA                "inflammation"   
#>  [4269] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4273] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [4277] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4281] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [4285] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [4289] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4293] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [4297] NA                NA                NA                NA               
#>  [4301] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4305] NA                "no inflammation" NA                NA               
#>  [4309] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4313] NA                NA                "inflammation"    "no inflammation"
#>  [4317] "inflammation"    NA                "no inflammation" NA               
#>  [4321] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4325] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4329] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4333] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4337] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4341] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [4345] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4349] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4353] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4357] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [4361] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4365] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4369] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [4373] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4377] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4381] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4385] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4389] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4393] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4397] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4401] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4405] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4409] "inflammation"    NA                "inflammation"    NA               
#>  [4413] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4417] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [4421] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [4425] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4429] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [4433] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4437] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [4441] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4445] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4449] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4453] NA                NA                "no inflammation" NA               
#>  [4457] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [4461] NA                NA                "no inflammation" "no inflammation"
#>  [4465] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4469] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4473] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4477] NA                "no inflammation" NA                "inflammation"   
#>  [4481] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [4485] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4489] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [4493] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4497] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [4501] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [4505] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4509] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [4513] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [4517] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [4521] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [4525] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4533] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [4537] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4541] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4545] "inflammation"    "no inflammation" NA                NA               
#>  [4549] "no inflammation" NA                NA                "inflammation"   
#>  [4553] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [4557] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [4561] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [4565] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [4569] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4573] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [4577] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4581] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [4585] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4589] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [4593] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [4597] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [4601] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4605] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [4609] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [4613] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [4617] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4621] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4625] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4629] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4633] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [4637] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4641] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4645] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4649] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [4653] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4657] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4661] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [4665] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4669] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4673] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4677] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4681] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4685] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [4689] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4693] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [4697] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4701] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4705] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4709] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4713] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4717] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4721] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4725] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4729] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4733] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4737] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4741] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4745] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4749] "no inflammation" NA                "no inflammation" NA               
#>  [4753] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4757] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [4761] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4765] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [4769] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4773] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [4777] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [4781] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4785] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4789] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4793] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4797] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [4801] "no inflammation" "inflammation"    NA                NA               
#>  [4805] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4809] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [4813] NA                NA                "no inflammation" NA               
#>  [4817] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [4821] "no inflammation" "no inflammation" NA                NA               
#>  [4825] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4829] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4833] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4837] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [4841] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4845] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [4849] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4853] "no inflammation" "no inflammation" NA                NA               
#>  [4857] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [4861] "no inflammation" "no inflammation" NA                NA               
#>  [4865] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [4869] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [4873] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4877] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4881] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4885] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4889] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4893] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [4897] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [4901] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4905] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [4909] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [4913] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [4917] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4921] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [4925] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4929] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4933] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [4937] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [4941] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4945] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4949] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [4953] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [4957] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4961] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [4965] "no inflammation" NA                NA                "inflammation"   
#>  [4969] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4973] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [4977] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4981] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [4985] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [4989] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [4993] "inflammation"    NA                "no inflammation" NA               
#>  [4997] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5001] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5005] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [5009] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5013] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [5017] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5021] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [5025] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5029] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [5033] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5037] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [5041] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [5045] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5049] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5053] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5057] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5061] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5065] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5069] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5073] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [5077] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5081] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5085] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [5089] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [5093] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5097] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [5101] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [5105] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [5109] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5113] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [5117] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5121] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5125] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5129] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5133] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5137] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [5141] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5145] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5149] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [5153] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5157] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [5161] "inflammation"    NA                NA                "no inflammation"
#>  [5165] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [5169] NA                "no inflammation" NA                "no inflammation"
#>  [5173] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [5177] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [5181] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5185] "no inflammation" "inflammation"    NA                NA               
#>  [5189] "no inflammation" NA                NA                "no inflammation"
#>  [5193] "inflammation"    "inflammation"    NA                NA               
#>  [5197] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5201] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [5205] "inflammation"    "no inflammation" NA                NA               
#>  [5209] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [5213] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5217] NA                NA                NA                "inflammation"   
#>  [5221] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5225] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [5229] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5233] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [5237] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [5241] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [5245] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [5249] "no inflammation" "inflammation"    NA                NA               
#>  [5253] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5257] NA                "inflammation"    "no inflammation" NA               
#>  [5261] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5265] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [5269] NA                "no inflammation" "no inflammation" NA               
#>  [5273] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [5277] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [5281] NA                "no inflammation" NA                "no inflammation"
#>  [5285] NA                NA                "inflammation"    "no inflammation"
#>  [5289] "inflammation"    "no inflammation" NA                NA               
#>  [5293] NA                NA                NA                "inflammation"   
#>  [5297] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [5301] "inflammation"    NA                NA                NA               
#>  [5305] NA                "no inflammation" "inflammation"    NA               
#>  [5309] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [5313] NA                NA                "inflammation"    NA               
#>  [5317] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [5321] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5325] "no inflammation" "no inflammation" NA                NA               
#>  [5329] "inflammation"    "no inflammation" NA                NA               
#>  [5333] NA                NA                NA                "no inflammation"
#>  [5337] "inflammation"    NA                "no inflammation" NA               
#>  [5341] NA                NA                NA                NA               
#>  [5345] "no inflammation" NA                NA                "inflammation"   
#>  [5349] "inflammation"    NA                "inflammation"    NA               
#>  [5353] NA                "no inflammation" NA                "inflammation"   
#>  [5357] NA                NA                NA                "no inflammation"
#>  [5361] NA                NA                "no inflammation" NA               
#>  [5365] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [5369] "inflammation"    NA                "inflammation"    NA               
#>  [5373] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [5377] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [5381] NA                NA                "inflammation"    "inflammation"   
#>  [5385] "inflammation"    "inflammation"    NA                NA               
#>  [5389] NA                "no inflammation" "no inflammation" NA               
#>  [5393] NA                NA                "inflammation"    "no inflammation"
#>  [5397] NA                NA                "inflammation"    NA               
#>  [5401] "inflammation"    "inflammation"    NA                NA               
#>  [5405] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [5409] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5413] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [5417] NA                NA                "no inflammation" "inflammation"   
#>  [5421] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [5425] "inflammation"    "no inflammation" NA                NA               
#>  [5429] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [5433] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5437] NA                "no inflammation" "no inflammation" NA               
#>  [5441] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [5445] "inflammation"    NA                "inflammation"    NA               
#>  [5449] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [5453] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [5457] "inflammation"    NA                "no inflammation" NA               
#>  [5461] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [5465] "inflammation"    NA                NA                NA               
#>  [5469] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5473] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5477] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [5481] "inflammation"    NA                "inflammation"    NA               
#>  [5485] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5489] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [5493] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [5497] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5501] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5505] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5509] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [5513] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [5517] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [5521] "inflammation"    NA                NA                "inflammation"   
#>  [5525] NA                "no inflammation" NA                "no inflammation"
#>  [5529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5533] "no inflammation" NA                NA                "no inflammation"
#>  [5537] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [5541] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [5545] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5549] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5553] NA                NA                "no inflammation" "no inflammation"
#>  [5557] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [5561] NA                "inflammation"    NA                "inflammation"   
#>  [5565] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [5569] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [5573] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [5577] "no inflammation" NA                NA                "inflammation"   
#>  [5581] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5585] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [5589] NA                NA                "no inflammation" NA               
#>  [5593] "no inflammation" "inflammation"    NA                NA               
#>  [5597] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [5601] NA                NA                "inflammation"    "inflammation"   
#>  [5605] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [5609] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [5613] "inflammation"    NA                NA                "no inflammation"
#>  [5617] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [5621] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [5625] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [5629] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [5633] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [5637] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [5641] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [5645] NA                NA                "inflammation"    NA               
#>  [5649] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [5653] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [5657] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [5661] NA                "inflammation"    "inflammation"    NA               
#>  [5665] "no inflammation" NA                "no inflammation" NA               
#>  [5669] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [5673] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5677] NA                "inflammation"    NA                "no inflammation"
#>  [5681] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [5685] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [5689] NA                NA                "no inflammation" "no inflammation"
#>  [5693] NA                "inflammation"    NA                NA               
#>  [5697] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5701] NA                "inflammation"    "no inflammation" NA               
#>  [5705] "no inflammation" NA                NA                "inflammation"   
#>  [5709] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [5713] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [5717] NA                "no inflammation" "inflammation"    NA               
#>  [5721] "inflammation"    NA                NA                "no inflammation"
#>  [5725] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [5729] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [5733] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [5737] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5741] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [5745] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5749] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [5753] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5757] "inflammation"    NA                NA                "no inflammation"
#>  [5761] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5765] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [5769] "no inflammation" "inflammation"    NA                NA               
#>  [5773] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5777] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [5781] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [5785] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [5789] "inflammation"    "inflammation"    NA                NA               
#>  [5793] "no inflammation" NA                "inflammation"    NA               
#>  [5797] NA                "no inflammation" NA                NA               
#>  [5801] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5805] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [5809] NA                NA                NA                NA               
#>  [5813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5817] NA                "inflammation"    "no inflammation" NA               
#>  [5821] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [5825] "no inflammation" NA                NA                NA               
#>  [5829] NA                NA                NA                NA               
#>  [5833] NA                NA                NA                NA               
#>  [5837] NA                NA                NA                NA               
#>  [5841] NA                NA                NA                NA               
#>  [5845] NA                NA                NA                NA               
#>  [5849] NA                NA                NA                NA               
#>  [5853] NA                NA                NA                NA               
#>  [5857] NA                NA                NA                NA               
#>  [5861] NA                NA                NA                NA               
#>  [5865] NA                NA                "no inflammation" "no inflammation"
#>  [5869] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [5873] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [5877] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [5881] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [5885] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [5889] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [5893] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [5897] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [5901] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [5905] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5909] "no inflammation" "inflammation"    NA                NA               
#>  [5913] NA                "inflammation"    NA                "no inflammation"
#>  [5917] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [5921] "inflammation"    NA                NA                "inflammation"   
#>  [5925] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [5929] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [5933] NA                NA                "inflammation"    NA               
#>  [5937] NA                NA                NA                "no inflammation"
#>  [5941] "no inflammation" NA                NA                "no inflammation"
#>  [5945] "inflammation"    NA                "no inflammation" NA               
#>  [5949] NA                NA                NA                "inflammation"   
#>  [5953] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [5957] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5961] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [5965] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [5969] "no inflammation" NA                "no inflammation" NA               
#>  [5973] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [5977] "inflammation"    "inflammation"    NA                "no inflammation"
#>  [5981] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [5985] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [5989] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [5993] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [5997] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6001] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6005] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [6009] "no inflammation" "no inflammation" NA                NA               
#>  [6013] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [6017] "inflammation"    "no inflammation" NA                NA               
#>  [6021] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [6025] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [6029] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6033] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6037] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [6041] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6045] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [6049] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6053] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [6057] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [6061] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [6065] "inflammation"    NA                "inflammation"    NA               
#>  [6069] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6073] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [6077] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6081] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [6085] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [6089] "inflammation"    NA                NA                "inflammation"   
#>  [6093] "inflammation"    NA                NA                "inflammation"   
#>  [6097] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [6101] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [6105] "inflammation"    NA                NA                "inflammation"   
#>  [6109] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [6113] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6117] NA                NA                NA                "inflammation"   
#>  [6121] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [6125] NA                "inflammation"    "inflammation"    NA               
#>  [6129] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6133] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6137] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [6141] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6145] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [6149] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [6153] "inflammation"    NA                "no inflammation" NA               
#>  [6157] "inflammation"    "inflammation"    NA                "inflammation"   
#>  [6161] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [6165] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [6169] "inflammation"    NA                "inflammation"    "inflammation"   
#>  [6173] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6177] NA                NA                "inflammation"    "inflammation"   
#>  [6181] NA                "inflammation"    "inflammation"    NA               
#>  [6185] "inflammation"    "inflammation"    "inflammation"    NA               
#>  [6189] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6193] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6197] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6201] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [6205] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6209] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6213] "inflammation"    NA                NA                NA               
#>  [6217] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [6221] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6225] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6229] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [6233] "no inflammation" NA                NA                NA               
#>  [6237] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [6241] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6245] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6249] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [6253] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6257] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6261] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6265] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [6269] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6273] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6277] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [6281] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6285] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [6289] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6297] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6301] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6305] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [6309] "no inflammation" "inflammation"    NA                "inflammation"   
#>  [6313] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [6317] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [6321] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6325] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [6329] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [6333] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6337] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6341] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6345] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6349] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [6353] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [6357] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6361] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [6365] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6369] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6373] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6377] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6381] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6385] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6389] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6393] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6397] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [6401] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6405] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6409] NA                "inflammation"    "inflammation"    "inflammation"   
#>  [6413] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6417] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6421] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6425] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6429] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [6433] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [6437] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6441] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6445] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6449] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6453] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6457] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [6461] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6465] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [6469] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6473] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6477] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6481] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [6485] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6489] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6493] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6497] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6501] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6505] "no inflammation" "no inflammation" NA                NA               
#>  [6509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6513] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6517] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6521] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6525] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6529] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [6533] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6537] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6541] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6545] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6549] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6553] "no inflammation" NA                NA                "no inflammation"
#>  [6557] "no inflammation" "no inflammation" NA                NA               
#>  [6561] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6565] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6569] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6573] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6577] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6581] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6585] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [6589] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6593] NA                "inflammation"    "no inflammation" NA               
#>  [6597] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6601] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6605] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6609] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6613] "no inflammation" NA                "no inflammation" NA               
#>  [6617] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [6621] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6625] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6629] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6633] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6637] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [6641] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6645] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6649] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6653] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6657] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [6661] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6665] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [6669] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6673] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [6677] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6681] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6685] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6689] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6693] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [6697] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6701] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [6705] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [6709] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [6713] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6717] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [6721] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [6725] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6729] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6733] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [6737] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6741] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6745] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [6749] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6753] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6757] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6761] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [6765] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6769] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6773] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6777] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6781] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [6785] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6789] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [6793] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6797] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6801] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6805] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6809] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6817] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6821] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [6825] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [6829] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6833] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6837] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6841] NA                "inflammation"    "no inflammation" NA               
#>  [6845] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6849] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6853] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6857] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6861] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6865] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6869] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6873] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [6877] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6881] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [6885] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6889] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6893] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6897] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6901] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [6905] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6909] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6913] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6917] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6921] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6925] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6929] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6933] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6937] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [6941] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [6945] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [6949] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [6953] "no inflammation" NA                NA                "no inflammation"
#>  [6957] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [6961] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [6965] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [6969] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#>  [6973] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6977] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [6981] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6985] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [6989] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [6993] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [6997] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7001] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7005] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7009] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7021] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7025] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7029] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7033] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7037] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7041] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7045] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7049] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [7053] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7057] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7061] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7065] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7069] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [7073] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [7077] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7081] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7085] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7089] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7093] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7097] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7101] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7105] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7109] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7113] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [7117] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7121] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7125] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7129] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [7133] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7137] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7141] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7145] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7149] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [7153] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [7157] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7161] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7165] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [7169] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7173] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7177] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7181] "no inflammation" NA                "inflammation"    NA               
#>  [7185] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7189] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [7193] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [7197] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [7201] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [7205] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [7209] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7213] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7217] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [7221] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7225] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7229] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [7233] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7237] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7241] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [7245] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7249] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7253] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7257] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7261] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7265] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7269] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7273] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7277] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [7281] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7285] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [7289] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7293] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [7297] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7301] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7305] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7309] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7313] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7317] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7321] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7325] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7329] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [7333] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7337] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7341] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7349] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7353] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [7357] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7361] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7365] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7369] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7373] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7377] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7381] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7385] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7389] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7393] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7397] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7401] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7405] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7409] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [7413] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7417] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7421] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7425] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [7429] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7433] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7437] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [7441] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [7445] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7449] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7453] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7457] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7461] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7465] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7469] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7473] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7477] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [7481] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [7485] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7489] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7493] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7497] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7501] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7505] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7509] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7513] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7517] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [7521] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7525] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7533] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7537] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7541] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7545] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7549] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7553] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7557] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7561] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7565] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7569] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7577] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [7581] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [7585] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7589] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [7593] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7597] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7601] "inflammation"    NA                "inflammation"    "no inflammation"
#>  [7605] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [7609] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7613] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7617] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [7621] NA                "no inflammation" NA                NA               
#>  [7625] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7629] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7633] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7637] "no inflammation" NA                "no inflammation" NA               
#>  [7641] NA                "no inflammation" NA                NA               
#>  [7645] "no inflammation" NA                NA                NA               
#>  [7649] NA                NA                "no inflammation" "inflammation"   
#>  [7653] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7657] NA                NA                NA                NA               
#>  [7661] "no inflammation" NA                NA                NA               
#>  [7665] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [7669] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7673] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7677] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7681] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7685] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7689] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [7693] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7697] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7701] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7705] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7709] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7713] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7717] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7721] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7725] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [7729] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7733] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [7737] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7741] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7745] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [7749] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7753] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7757] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7761] "no inflammation" NA                NA                "no inflammation"
#>  [7765] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [7769] NA                "inflammation"    "no inflammation" NA               
#>  [7773] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [7777] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7781] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7785] "no inflammation" NA                "no inflammation" NA               
#>  [7789] "inflammation"    "no inflammation" NA                NA               
#>  [7793] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7797] NA                "no inflammation" "no inflammation" NA               
#>  [7801] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7805] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7809] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [7813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7817] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [7821] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [7825] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [7829] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7833] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7837] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [7841] NA                "no inflammation" NA                "no inflammation"
#>  [7845] NA                "no inflammation" NA                "no inflammation"
#>  [7849] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7853] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7857] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7861] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7865] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [7869] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7873] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7877] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [7881] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [7885] "inflammation"    NA                "no inflammation" NA               
#>  [7889] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7893] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7897] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7901] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [7905] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#>  [7909] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [7913] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [7917] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7921] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [7925] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [7929] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [7933] NA                NA                "no inflammation" "no inflammation"
#>  [7937] NA                "no inflammation" "inflammation"    NA               
#>  [7941] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [7945] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [7949] NA                "no inflammation" "no inflammation" NA               
#>  [7953] "no inflammation" "no inflammation" NA                NA               
#>  [7957] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7961] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [7965] "inflammation"    NA                NA                NA               
#>  [7969] NA                NA                "no inflammation" NA               
#>  [7973] NA                "no inflammation" "no inflammation" NA               
#>  [7977] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [7981] "no inflammation" NA                NA                "no inflammation"
#>  [7985] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [7989] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [7993] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [7997] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8001] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8005] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8009] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8021] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8025] "no inflammation" "no inflammation" NA                NA               
#>  [8029] NA                "no inflammation" "no inflammation" NA               
#>  [8033] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8037] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8041] NA                "no inflammation" "no inflammation" NA               
#>  [8045] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8049] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8053] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8057] NA                NA                "no inflammation" "no inflammation"
#>  [8061] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [8065] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8069] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8073] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8077] NA                NA                "no inflammation" "no inflammation"
#>  [8081] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8085] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8089] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8093] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8097] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8101] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8105] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8109] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8113] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8117] "no inflammation" "no inflammation" NA                NA               
#>  [8121] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8125] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8129] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8133] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8137] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8141] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [8145] "no inflammation" NA                NA                "no inflammation"
#>  [8149] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8153] NA                "no inflammation" NA                "no inflammation"
#>  [8157] NA                "no inflammation" NA                "inflammation"   
#>  [8161] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [8165] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [8169] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8173] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8177] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [8181] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8185] "no inflammation" NA                NA                "inflammation"   
#>  [8189] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8193] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8197] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8201] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [8205] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8209] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8213] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [8217] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8221] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8225] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [8229] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8233] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8237] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8241] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8245] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [8249] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8253] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8257] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8261] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8265] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [8269] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [8273] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8277] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [8281] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8285] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [8289] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8297] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [8301] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8305] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8309] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8313] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8317] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8321] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8325] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8329] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8333] NA                NA                "no inflammation" "no inflammation"
#>  [8337] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8341] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8349] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8353] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8357] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8361] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [8365] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8369] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8373] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8377] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8381] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [8385] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#>  [8389] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8393] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8397] NA                NA                NA                NA               
#>  [8401] "no inflammation" NA                "no inflammation" NA               
#>  [8405] NA                "inflammation"    NA                NA               
#>  [8409] NA                "no inflammation" "no inflammation" NA               
#>  [8413] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8417] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8421] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8425] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8429] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8433] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8437] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [8441] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8445] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8449] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8453] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8457] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8461] "inflammation"    NA                NA                NA               
#>  [8465] "no inflammation" "no inflammation" NA                NA               
#>  [8469] NA                "inflammation"    NA                NA               
#>  [8473] "no inflammation" NA                NA                "inflammation"   
#>  [8477] NA                "no inflammation" NA                "no inflammation"
#>  [8481] NA                "no inflammation" "no inflammation" NA               
#>  [8485] NA                NA                "inflammation"    "inflammation"   
#>  [8489] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [8493] NA                NA                "inflammation"    "no inflammation"
#>  [8497] "inflammation"    NA                NA                "no inflammation"
#>  [8501] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8505] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8509] "no inflammation" "no inflammation" NA                NA               
#>  [8513] NA                NA                "no inflammation" NA               
#>  [8517] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [8521] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8525] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [8529] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8533] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [8537] NA                NA                NA                NA               
#>  [8541] NA                NA                NA                NA               
#>  [8545] NA                NA                "no inflammation" "inflammation"   
#>  [8549] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [8553] NA                "inflammation"    "no inflammation" NA               
#>  [8557] NA                NA                "no inflammation" NA               
#>  [8561] "no inflammation" "no inflammation" NA                NA               
#>  [8565] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [8569] NA                NA                "no inflammation" "no inflammation"
#>  [8573] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8577] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [8581] NA                "inflammation"    "no inflammation" NA               
#>  [8585] "no inflammation" "no inflammation" NA                NA               
#>  [8589] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8593] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [8597] "no inflammation" "no inflammation" NA                NA               
#>  [8601] "no inflammation" NA                "no inflammation" NA               
#>  [8605] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8609] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8613] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8617] "inflammation"    NA                "no inflammation" "no inflammation"
#>  [8621] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8625] "inflammation"    NA                NA                NA               
#>  [8629] NA                "no inflammation" "no inflammation" NA               
#>  [8633] "no inflammation" NA                "no inflammation" NA               
#>  [8637] "no inflammation" NA                NA                NA               
#>  [8641] NA                "no inflammation" NA                "no inflammation"
#>  [8645] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8649] NA                "inflammation"    NA                NA               
#>  [8653] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8657] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [8661] NA                "no inflammation" NA                "no inflammation"
#>  [8665] NA                "no inflammation" "inflammation"    NA               
#>  [8669] "no inflammation" NA                NA                "no inflammation"
#>  [8673] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [8677] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8681] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8685] "inflammation"    "no inflammation" NA                NA               
#>  [8689] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8693] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [8697] NA                NA                "inflammation"    NA               
#>  [8701] "inflammation"    "no inflammation" NA                NA               
#>  [8705] NA                NA                "no inflammation" "no inflammation"
#>  [8709] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [8713] "inflammation"    NA                NA                "no inflammation"
#>  [8717] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8721] NA                "no inflammation" NA                "inflammation"   
#>  [8725] NA                "inflammation"    NA                NA               
#>  [8729] "no inflammation" "no inflammation" NA                NA               
#>  [8733] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8737] NA                NA                "no inflammation" "no inflammation"
#>  [8741] "no inflammation" "no inflammation" NA                "inflammation"   
#>  [8745] "no inflammation" NA                "no inflammation" NA               
#>  [8749] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8753] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8757] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8761] "no inflammation" "no inflammation" NA                NA               
#>  [8765] NA                NA                NA                NA               
#>  [8769] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [8773] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8777] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8781] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [8785] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8789] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8793] "no inflammation" NA                "no inflammation" NA               
#>  [8797] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8801] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [8805] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8809] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8813] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8817] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [8821] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [8825] "no inflammation" NA                "inflammation"    "inflammation"   
#>  [8829] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [8833] NA                NA                "inflammation"    "no inflammation"
#>  [8837] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8841] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [8845] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8849] NA                "inflammation"    NA                "no inflammation"
#>  [8853] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8857] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [8861] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8865] NA                "inflammation"    "no inflammation" NA               
#>  [8869] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8873] NA                NA                NA                "no inflammation"
#>  [8877] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8881] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [8885] NA                "inflammation"    "no inflammation" NA               
#>  [8889] "no inflammation" NA                "no inflammation" NA               
#>  [8893] NA                "no inflammation" "no inflammation" NA               
#>  [8897] NA                "inflammation"    NA                NA               
#>  [8901] NA                "inflammation"    "no inflammation" NA               
#>  [8905] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [8909] "no inflammation" NA                NA                "no inflammation"
#>  [8913] NA                NA                NA                "inflammation"   
#>  [8917] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8921] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8925] "no inflammation" NA                NA                NA               
#>  [8929] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [8933] NA                "no inflammation" NA                "no inflammation"
#>  [8937] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8941] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8945] NA                NA                "no inflammation" NA               
#>  [8949] NA                "no inflammation" "inflammation"    NA               
#>  [8953] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#>  [8957] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [8961] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [8965] NA                NA                "no inflammation" "inflammation"   
#>  [8969] NA                "inflammation"    NA                NA               
#>  [8973] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [8977] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [8981] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [8985] NA                NA                "inflammation"    "no inflammation"
#>  [8989] "no inflammation" NA                "inflammation"    NA               
#>  [8993] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [8997] NA                NA                "no inflammation" NA               
#>  [9001] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9005] "no inflammation" "no inflammation" NA                NA               
#>  [9009] NA                NA                "no inflammation" "inflammation"   
#>  [9013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9017] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [9021] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9025] "inflammation"    NA                "no inflammation" NA               
#>  [9029] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [9033] NA                "no inflammation" "no inflammation" NA               
#>  [9037] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [9041] NA                "no inflammation" "no inflammation" NA               
#>  [9045] NA                NA                "inflammation"    "no inflammation"
#>  [9049] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [9053] "no inflammation" "no inflammation" NA                NA               
#>  [9057] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9061] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9065] NA                NA                "inflammation"    "inflammation"   
#>  [9069] NA                "inflammation"    NA                NA               
#>  [9073] "no inflammation" "no inflammation" NA                NA               
#>  [9077] NA                "inflammation"    NA                "inflammation"   
#>  [9081] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9085] NA                "no inflammation" "no inflammation" NA               
#>  [9089] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9093] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9097] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9101] "no inflammation" NA                NA                NA               
#>  [9105] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9109] "inflammation"    "no inflammation" NA                NA               
#>  [9113] NA                "no inflammation" "no inflammation" NA               
#>  [9117] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9121] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9125] "inflammation"    NA                NA                NA               
#>  [9129] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [9133] "inflammation"    NA                NA                "no inflammation"
#>  [9137] NA                "inflammation"    "no inflammation" "inflammation"   
#>  [9141] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9145] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9149] NA                "inflammation"    "inflammation"    "no inflammation"
#>  [9153] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [9157] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9161] "inflammation"    NA                NA                "no inflammation"
#>  [9165] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9169] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9173] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9177] NA                NA                "inflammation"    "no inflammation"
#>  [9181] NA                "no inflammation" "inflammation"    "no inflammation"
#>  [9185] "inflammation"    NA                "no inflammation" "inflammation"   
#>  [9189] "no inflammation" NA                NA                NA               
#>  [9193] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9197] NA                NA                "no inflammation" NA               
#>  [9201] "no inflammation" "no inflammation" NA                NA               
#>  [9205] NA                "no inflammation" "inflammation"    NA               
#>  [9209] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9213] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9217] "no inflammation" NA                "inflammation"    NA               
#>  [9221] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9225] "no inflammation" "no inflammation" NA                NA               
#>  [9229] "no inflammation" NA                "no inflammation" "inflammation"   
#>  [9233] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9237] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9241] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [9245] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9249] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#>  [9253] NA                NA                NA                "inflammation"   
#>  [9257] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9261] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9265] NA                NA                NA                "no inflammation"
#>  [9269] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9273] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9277] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9281] "no inflammation" NA                NA                "inflammation"   
#>  [9285] "no inflammation" NA                "no inflammation" NA               
#>  [9289] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#>  [9293] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9297] NA                "no inflammation" NA                NA               
#>  [9301] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9305] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9309] NA                "no inflammation" NA                NA               
#>  [9313] "no inflammation" "no inflammation" NA                NA               
#>  [9317] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [9321] "inflammation"    "no inflammation" NA                "inflammation"   
#>  [9325] "no inflammation" NA                NA                "no inflammation"
#>  [9329] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [9333] NA                "no inflammation" "no inflammation" NA               
#>  [9337] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [9341] "no inflammation" NA                "no inflammation" NA               
#>  [9345] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [9349] NA                "no inflammation" NA                "no inflammation"
#>  [9353] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [9357] NA                "no inflammation" NA                NA               
#>  [9361] "no inflammation" NA                NA                NA               
#>  [9365] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9369] NA                NA                "no inflammation" NA               
#>  [9373] NA                "no inflammation" NA                NA               
#>  [9377] NA                "no inflammation" "no inflammation" NA               
#>  [9381] "no inflammation" NA                NA                NA               
#>  [9385] "no inflammation" NA                NA                NA               
#>  [9389] NA                NA                "inflammation"    "no inflammation"
#>  [9393] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9397] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#>  [9401] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9405] NA                NA                "no inflammation" NA               
#>  [9409] NA                "no inflammation" NA                NA               
#>  [9413] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9417] "inflammation"    "inflammation"    NA                NA               
#>  [9421] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9425] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9429] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [9433] NA                "inflammation"    NA                "no inflammation"
#>  [9437] "no inflammation" "no inflammation" NA                NA               
#>  [9441] NA                "no inflammation" NA                "no inflammation"
#>  [9445] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9449] NA                NA                NA                NA               
#>  [9453] NA                "no inflammation" NA                "inflammation"   
#>  [9457] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [9461] "inflammation"    "inflammation"    "no inflammation" NA               
#>  [9465] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9469] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9473] "no inflammation" NA                "no inflammation" NA               
#>  [9477] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9481] NA                "inflammation"    NA                "inflammation"   
#>  [9485] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9489] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9493] "inflammation"    NA                "inflammation"    NA               
#>  [9497] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#>  [9501] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [9505] NA                NA                "inflammation"    "inflammation"   
#>  [9509] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#>  [9513] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [9517] NA                NA                NA                NA               
#>  [9521] NA                NA                "no inflammation" NA               
#>  [9525] "no inflammation" NA                NA                NA               
#>  [9529] NA                NA                "no inflammation" NA               
#>  [9533] "no inflammation" NA                "inflammation"    NA               
#>  [9537] "no inflammation" NA                NA                NA               
#>  [9541] NA                NA                NA                "no inflammation"
#>  [9545] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9549] "no inflammation" NA                NA                "no inflammation"
#>  [9553] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9557] "inflammation"    NA                "no inflammation" NA               
#>  [9561] "no inflammation" NA                "no inflammation" NA               
#>  [9565] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9569] "no inflammation" NA                NA                "no inflammation"
#>  [9573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9577] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [9581] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9585] NA                "no inflammation" NA                "no inflammation"
#>  [9589] "no inflammation" "inflammation"    NA                "no inflammation"
#>  [9593] "no inflammation" "inflammation"    "inflammation"    NA               
#>  [9597] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9601] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [9605] NA                "inflammation"    NA                "inflammation"   
#>  [9609] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9613] NA                "no inflammation" NA                "no inflammation"
#>  [9617] "no inflammation" "no inflammation" NA                NA               
#>  [9621] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [9625] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9629] NA                NA                NA                "no inflammation"
#>  [9633] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9637] "no inflammation" NA                "no inflammation" NA               
#>  [9641] NA                "no inflammation" NA                "no inflammation"
#>  [9645] NA                "no inflammation" NA                NA               
#>  [9649] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9653] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9657] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9661] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9665] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9669] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9673] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [9677] "no inflammation" NA                "no inflammation" NA               
#>  [9681] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9685] NA                NA                "no inflammation" NA               
#>  [9689] "no inflammation" NA                NA                "no inflammation"
#>  [9693] "inflammation"    "no inflammation" "inflammation"    NA               
#>  [9697] NA                NA                NA                NA               
#>  [9701] "inflammation"    "no inflammation" NA                NA               
#>  [9705] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#>  [9709] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9713] "no inflammation" NA                "no inflammation" NA               
#>  [9717] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9721] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9725] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9729] "no inflammation" NA                NA                NA               
#>  [9733] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9737] NA                NA                NA                NA               
#>  [9741] "no inflammation" "inflammation"    "no inflammation" NA               
#>  [9745] "inflammation"    "no inflammation" NA                "no inflammation"
#>  [9749] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9753] NA                "no inflammation" "no inflammation" NA               
#>  [9757] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9761] NA                "no inflammation" NA                "no inflammation"
#>  [9765] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9769] "no inflammation" NA                NA                "no inflammation"
#>  [9773] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9777] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9781] NA                "no inflammation" NA                "no inflammation"
#>  [9785] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [9789] "no inflammation" "no inflammation" "inflammation"    NA               
#>  [9793] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9797] NA                "no inflammation" "inflammation"    "inflammation"   
#>  [9801] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#>  [9805] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9809] "no inflammation" NA                "inflammation"    "no inflammation"
#>  [9813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9817] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9821] "inflammation"    "no inflammation" "no inflammation" NA               
#>  [9825] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9829] "no inflammation" "inflammation"    NA                NA               
#>  [9833] "no inflammation" NA                "no inflammation" NA               
#>  [9837] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9841] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9845] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9849] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9853] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9857] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#>  [9861] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9865] NA                "no inflammation" NA                NA               
#>  [9869] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#>  [9873] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9877] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [9881] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#>  [9885] NA                NA                "no inflammation" NA               
#>  [9889] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#>  [9893] "no inflammation" "no inflammation" NA                "no inflammation"
#>  [9897] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9901] NA                "inflammation"    "no inflammation" "no inflammation"
#>  [9905] "inflammation"    NA                NA                "inflammation"   
#>  [9909] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9913] NA                "no inflammation" "no inflammation" NA               
#>  [9917] NA                "no inflammation" NA                "no inflammation"
#>  [9921] NA                NA                "no inflammation" "no inflammation"
#>  [9925] "no inflammation" NA                NA                NA               
#>  [9929] NA                "no inflammation" "no inflammation" "inflammation"   
#>  [9933] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9937] NA                "no inflammation" NA                "no inflammation"
#>  [9941] NA                NA                "no inflammation" NA               
#>  [9945] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9949] "no inflammation" NA                NA                "no inflammation"
#>  [9953] "no inflammation" NA                "no inflammation" NA               
#>  [9957] "no inflammation" "no inflammation" "no inflammation" NA               
#>  [9961] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9965] NA                "no inflammation" "no inflammation" "no inflammation"
#>  [9969] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#>  [9973] "no inflammation" NA                NA                "no inflammation"
#>  [9977] NA                "no inflammation" NA                NA               
#>  [9981] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9985] "no inflammation" NA                NA                NA               
#>  [9989] "no inflammation" NA                "no inflammation" "no inflammation"
#>  [9993] NA                "no inflammation" NA                "no inflammation"
#>  [9997] NA                "no inflammation" "no inflammation" NA               
#> [10001] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10005] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10009] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10013] NA                "inflammation"    "no inflammation" "no inflammation"
#> [10017] NA                "inflammation"    NA                "no inflammation"
#> [10021] NA                NA                "inflammation"    NA               
#> [10025] NA                "inflammation"    "no inflammation" NA               
#> [10029] NA                "no inflammation" "inflammation"    NA               
#> [10033] NA                NA                "no inflammation" "no inflammation"
#> [10037] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10041] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [10045] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10049] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10053] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10057] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10061] "no inflammation" "no inflammation" NA                NA               
#> [10065] "no inflammation" NA                NA                NA               
#> [10069] NA                NA                "no inflammation" "no inflammation"
#> [10073] "no inflammation" NA                "no inflammation" NA               
#> [10077] NA                "no inflammation" NA                "no inflammation"
#> [10081] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10085] "no inflammation" NA                NA                NA               
#> [10089] "no inflammation" NA                "no inflammation" NA               
#> [10093] NA                "no inflammation" NA                "no inflammation"
#> [10097] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10101] "no inflammation" NA                "no inflammation" NA               
#> [10105] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10109] "no inflammation" NA                NA                NA               
#> [10113] NA                "no inflammation" "no inflammation" "inflammation"   
#> [10117] NA                NA                "no inflammation" "no inflammation"
#> [10121] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [10125] "inflammation"    "inflammation"    NA                NA               
#> [10129] "no inflammation" NA                NA                "inflammation"   
#> [10133] "inflammation"    NA                NA                "no inflammation"
#> [10137] NA                "no inflammation" "no inflammation" "inflammation"   
#> [10141] NA                "no inflammation" NA                "inflammation"   
#> [10145] "no inflammation" NA                "no inflammation" "inflammation"   
#> [10149] NA                "no inflammation" "no inflammation" "inflammation"   
#> [10153] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10157] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10161] "no inflammation" NA                "inflammation"    "no inflammation"
#> [10165] NA                NA                "no inflammation" NA               
#> [10169] "inflammation"    NA                "no inflammation" "inflammation"   
#> [10173] "no inflammation" NA                NA                "inflammation"   
#> [10177] "inflammation"    "no inflammation" "no inflammation" NA               
#> [10181] "no inflammation" "inflammation"    "no inflammation" NA               
#> [10185] NA                "inflammation"    "no inflammation" "no inflammation"
#> [10189] NA                NA                "no inflammation" "no inflammation"
#> [10193] "inflammation"    NA                NA                NA               
#> [10197] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10201] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10205] NA                NA                "no inflammation" "no inflammation"
#> [10209] NA                "inflammation"    "inflammation"    NA               
#> [10213] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10217] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [10221] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10225] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10229] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10233] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10237] "inflammation"    "inflammation"    NA                "no inflammation"
#> [10241] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10245] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [10249] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10253] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10257] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10261] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10265] "inflammation"    "no inflammation" "no inflammation" NA               
#> [10269] NA                "no inflammation" "no inflammation" NA               
#> [10273] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [10277] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10281] "inflammation"    "no inflammation" "inflammation"    NA               
#> [10285] "no inflammation" "inflammation"    NA                "no inflammation"
#> [10289] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10297] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10301] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10305] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10309] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10313] "inflammation"    "inflammation"    NA                "inflammation"   
#> [10317] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10321] "no inflammation" "no inflammation" NA                "inflammation"   
#> [10325] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10329] "inflammation"    "no inflammation" "no inflammation" NA               
#> [10333] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [10337] NA                "no inflammation" "inflammation"    "no inflammation"
#> [10341] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10349] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10353] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10357] NA                NA                "no inflammation" "no inflammation"
#> [10361] "no inflammation" NA                "no inflammation" NA               
#> [10365] "no inflammation" NA                "inflammation"    "no inflammation"
#> [10369] NA                "inflammation"    NA                "no inflammation"
#> [10373] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10377] NA                "inflammation"    "no inflammation" NA               
#> [10381] NA                "inflammation"    "no inflammation" "no inflammation"
#> [10385] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10389] "no inflammation" NA                "no inflammation" "inflammation"   
#> [10393] "no inflammation" NA                NA                NA               
#> [10397] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10401] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10405] NA                "no inflammation" NA                "inflammation"   
#> [10409] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [10413] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10417] "no inflammation" NA                NA                NA               
#> [10421] NA                NA                NA                "no inflammation"
#> [10425] NA                "no inflammation" NA                "no inflammation"
#> [10429] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10433] "no inflammation" "inflammation"    NA                "no inflammation"
#> [10437] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10441] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10445] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10449] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10453] NA                NA                "no inflammation" NA               
#> [10457] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10461] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10465] NA                "inflammation"    "no inflammation" "no inflammation"
#> [10469] "no inflammation" NA                "no inflammation" "inflammation"   
#> [10473] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10477] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10481] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10485] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10489] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10493] "inflammation"    NA                "no inflammation" NA               
#> [10497] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10501] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10505] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10513] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10517] "no inflammation" NA                "no inflammation" NA               
#> [10521] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10525] NA                NA                NA                "no inflammation"
#> [10529] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10533] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10537] "no inflammation" NA                "no inflammation" NA               
#> [10541] NA                "inflammation"    NA                NA               
#> [10545] NA                "no inflammation" NA                "no inflammation"
#> [10549] NA                "no inflammation" NA                "inflammation"   
#> [10553] "no inflammation" NA                "no inflammation" NA               
#> [10557] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10561] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [10565] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10569] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [10573] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10577] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10581] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10585] "no inflammation" "inflammation"    "no inflammation" NA               
#> [10589] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10593] "no inflammation" NA                "no inflammation" "inflammation"   
#> [10597] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10601] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [10605] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10609] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10613] "no inflammation" "inflammation"    NA                NA               
#> [10617] NA                NA                NA                "no inflammation"
#> [10621] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10625] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10629] NA                "no inflammation" NA                "inflammation"   
#> [10633] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10637] "no inflammation" "no inflammation" NA                NA               
#> [10641] "no inflammation" NA                NA                NA               
#> [10645] NA                "no inflammation" NA                "inflammation"   
#> [10649] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10653] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10657] NA                NA                NA                NA               
#> [10661] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10665] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10669] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10673] NA                "no inflammation" "inflammation"    NA               
#> [10677] "no inflammation" NA                NA                NA               
#> [10681] "no inflammation" "no inflammation" NA                NA               
#> [10685] NA                "no inflammation" NA                "no inflammation"
#> [10689] "no inflammation" NA                NA                NA               
#> [10693] NA                "inflammation"    NA                "inflammation"   
#> [10697] "no inflammation" "inflammation"    "no inflammation" NA               
#> [10701] NA                NA                "no inflammation" NA               
#> [10705] "inflammation"    NA                "no inflammation" "no inflammation"
#> [10709] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10713] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10717] NA                "no inflammation" "no inflammation" NA               
#> [10721] "no inflammation" NA                NA                NA               
#> [10725] NA                NA                "no inflammation" NA               
#> [10729] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10733] NA                NA                "no inflammation" NA               
#> [10737] NA                "inflammation"    "no inflammation" NA               
#> [10741] "inflammation"    "inflammation"    NA                "inflammation"   
#> [10745] NA                NA                NA                NA               
#> [10749] NA                NA                NA                "no inflammation"
#> [10753] NA                "no inflammation" NA                "no inflammation"
#> [10757] "no inflammation" NA                "no inflammation" NA               
#> [10761] NA                NA                NA                NA               
#> [10765] NA                NA                NA                "no inflammation"
#> [10769] "no inflammation" NA                "no inflammation" NA               
#> [10773] NA                NA                "no inflammation" "no inflammation"
#> [10777] "inflammation"    "no inflammation" "no inflammation" NA               
#> [10781] NA                "no inflammation" NA                NA               
#> [10785] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10789] NA                NA                NA                NA               
#> [10793] NA                NA                NA                NA               
#> [10797] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10801] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [10805] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10809] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10813] NA                "no inflammation" NA                "no inflammation"
#> [10817] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10821] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [10825] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10829] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10833] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [10837] "no inflammation" "inflammation"    NA                "no inflammation"
#> [10841] "no inflammation" "inflammation"    "no inflammation" NA               
#> [10845] "no inflammation" "no inflammation" NA                "no inflammation"
#> [10849] "no inflammation" NA                "inflammation"    "inflammation"   
#> [10853] "no inflammation" NA                "inflammation"    "inflammation"   
#> [10857] "no inflammation" NA                "no inflammation" NA               
#> [10861] NA                "no inflammation" NA                "no inflammation"
#> [10865] "no inflammation" NA                "inflammation"    "no inflammation"
#> [10869] "no inflammation" "no inflammation" NA                "inflammation"   
#> [10873] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [10877] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10881] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [10885] NA                "no inflammation" "no inflammation" NA               
#> [10889] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10893] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [10897] NA                "no inflammation" "no inflammation" NA               
#> [10901] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10905] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [10909] "inflammation"    "no inflammation" NA                NA               
#> [10913] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10917] NA                "no inflammation" NA                "no inflammation"
#> [10921] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [10925] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [10929] "no inflammation" "no inflammation" "no inflammation" NA               
#> [10933] "inflammation"    NA                "no inflammation" "no inflammation"
#> [10937] NA                "inflammation"    "no inflammation" NA               
#> [10941] "no inflammation" "no inflammation" "inflammation"    NA               
#> [10945] NA                "no inflammation" NA                "no inflammation"
#> [10949] NA                NA                "no inflammation" "inflammation"   
#> [10953] "no inflammation" "no inflammation" NA                NA               
#> [10957] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10961] "no inflammation" NA                "no inflammation" "no inflammation"
#> [10965] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [10969] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10973] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [10977] NA                "no inflammation" "no inflammation" "no inflammation"
#> [10981] "no inflammation" NA                "no inflammation" NA               
#> [10985] NA                "no inflammation" NA                "no inflammation"
#> [10989] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [10993] "no inflammation" NA                "no inflammation" "inflammation"   
#> [10997] "no inflammation" "no inflammation" NA                NA               
#> [11001] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11005] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11009] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11013] NA                "no inflammation" "inflammation"    "no inflammation"
#> [11017] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11021] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11025] "no inflammation" "no inflammation" NA                "inflammation"   
#> [11029] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11033] NA                "inflammation"    "inflammation"    "inflammation"   
#> [11037] "inflammation"    "no inflammation" NA                NA               
#> [11041] NA                "no inflammation" "no inflammation" "inflammation"   
#> [11045] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11049] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11053] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11057] "no inflammation" "inflammation"    NA                "no inflammation"
#> [11061] "no inflammation" NA                NA                "no inflammation"
#> [11065] "inflammation"    "inflammation"    NA                NA               
#> [11069] NA                NA                "inflammation"    "inflammation"   
#> [11073] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11077] "inflammation"    NA                NA                "no inflammation"
#> [11081] "inflammation"    NA                "no inflammation" "no inflammation"
#> [11085] NA                "inflammation"    "no inflammation" "no inflammation"
#> [11089] NA                NA                "no inflammation" NA               
#> [11093] NA                NA                "inflammation"    "inflammation"   
#> [11097] NA                "inflammation"    "inflammation"    "inflammation"   
#> [11101] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11105] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11109] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11113] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11117] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [11121] "no inflammation" NA                NA                NA               
#> [11125] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [11129] NA                "no inflammation" NA                NA               
#> [11133] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11137] NA                "inflammation"    "no inflammation" "no inflammation"
#> [11141] NA                NA                "no inflammation" NA               
#> [11145] "inflammation"    "no inflammation" "no inflammation" NA               
#> [11149] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11153] NA                NA                NA                "no inflammation"
#> [11157] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11161] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11165] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [11169] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11173] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11177] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11181] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [11185] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [11189] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11193] NA                "no inflammation" "inflammation"    NA               
#> [11197] "inflammation"    "no inflammation" NA                "inflammation"   
#> [11201] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [11205] "no inflammation" "no inflammation" NA                "inflammation"   
#> [11209] "inflammation"    "no inflammation" NA                "no inflammation"
#> [11213] "no inflammation" "no inflammation" NA                "inflammation"   
#> [11217] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [11221] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [11225] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11229] NA                NA                NA                "no inflammation"
#> [11233] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11237] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11241] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11245] NA                NA                "no inflammation" NA               
#> [11249] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11253] NA                "no inflammation" NA                "no inflammation"
#> [11257] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [11261] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11265] NA                "no inflammation" NA                "no inflammation"
#> [11269] "inflammation"    NA                "no inflammation" "no inflammation"
#> [11273] NA                "no inflammation" "inflammation"    "inflammation"   
#> [11277] NA                NA                NA                NA               
#> [11281] NA                "no inflammation" "no inflammation" "inflammation"   
#> [11285] "no inflammation" "no inflammation" NA                NA               
#> [11289] NA                NA                NA                "no inflammation"
#> [11293] NA                NA                "inflammation"    "no inflammation"
#> [11297] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11301] NA                "inflammation"    "inflammation"    NA               
#> [11305] "no inflammation" "no inflammation" NA                "inflammation"   
#> [11309] "no inflammation" "inflammation"    NA                NA               
#> [11313] NA                "inflammation"    NA                "no inflammation"
#> [11317] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [11321] NA                NA                "no inflammation" NA               
#> [11325] "inflammation"    "inflammation"    "inflammation"    NA               
#> [11329] NA                "no inflammation" NA                "inflammation"   
#> [11333] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11337] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11341] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11345] NA                NA                "inflammation"    "no inflammation"
#> [11349] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11353] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11357] NA                "inflammation"    "inflammation"    "no inflammation"
#> [11361] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11365] "no inflammation" "inflammation"    "no inflammation" NA               
#> [11369] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11373] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [11377] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [11381] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11385] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [11389] "inflammation"    "no inflammation" "no inflammation" NA               
#> [11393] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11397] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11401] NA                NA                NA                "no inflammation"
#> [11405] NA                "no inflammation" "no inflammation" "no inflammation"
#> [11409] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11413] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11417] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11421] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [11425] NA                "no inflammation" "inflammation"    "no inflammation"
#> [11429] NA                NA                "no inflammation" "inflammation"   
#> [11433] "no inflammation" "inflammation"    NA                NA               
#> [11437] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [11441] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11445] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11449] NA                "inflammation"    "no inflammation" NA               
#> [11453] NA                NA                NA                NA               
#> [11457] NA                NA                NA                NA               
#> [11461] NA                NA                NA                NA               
#> [11465] NA                "no inflammation" NA                NA               
#> [11469] NA                NA                NA                NA               
#> [11473] NA                NA                NA                NA               
#> [11477] NA                NA                "no inflammation" NA               
#> [11481] NA                NA                NA                NA               
#> [11485] NA                NA                NA                NA               
#> [11489] NA                NA                NA                "no inflammation"
#> [11493] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11497] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11501] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11505] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11509] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11513] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11517] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11521] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11525] "inflammation"    NA                "no inflammation" "no inflammation"
#> [11529] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11533] "inflammation"    "no inflammation" NA                NA               
#> [11537] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11541] "no inflammation" NA                "no inflammation" "inflammation"   
#> [11545] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11549] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [11553] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11557] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11561] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11565] "inflammation"    "no inflammation" NA                "no inflammation"
#> [11569] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11577] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11581] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11585] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11589] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11593] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11597] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11601] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11605] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [11609] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [11613] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11617] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [11621] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11625] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11629] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11633] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11637] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [11641] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11645] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [11649] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11653] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11657] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11661] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11665] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11669] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [11673] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11677] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11681] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11685] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11689] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11693] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11697] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11701] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11705] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11709] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11713] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [11717] "inflammation"    NA                "no inflammation" "inflammation"   
#> [11721] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11725] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11729] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11733] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11737] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11741] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11745] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [11749] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11753] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11757] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11761] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11765] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [11769] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11773] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11777] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11781] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [11785] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11789] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11793] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11797] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11801] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11805] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11809] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11813] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11817] "no inflammation" "no inflammation" NA                "no inflammation"
#> [11821] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11825] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11829] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11833] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11837] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11841] "no inflammation" NA                "no inflammation" "inflammation"   
#> [11845] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11849] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11853] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [11857] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11861] "no inflammation" "no inflammation" "no inflammation" NA               
#> [11865] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11869] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11873] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11877] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [11881] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11885] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11889] "no inflammation" NA                "no inflammation" "inflammation"   
#> [11893] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [11897] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11901] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11905] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [11909] "no inflammation" "inflammation"    "no inflammation" NA               
#> [11913] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11917] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11921] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11925] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11929] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11933] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11937] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [11941] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [11945] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11949] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11953] "inflammation"    "no inflammation" NA                "no inflammation"
#> [11957] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11961] "no inflammation" "no inflammation" NA                "inflammation"   
#> [11965] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11969] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [11973] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11977] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [11981] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [11985] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [11989] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [11993] "no inflammation" NA                "no inflammation" "no inflammation"
#> [11997] "no inflammation" "no inflammation" NA                "inflammation"   
#> [12001] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12005] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12009] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12013] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12021] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12025] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12029] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12033] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12037] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12041] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12045] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12049] "no inflammation" "no inflammation" "no inflammation" NA               
#> [12053] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12057] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12061] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12065] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12069] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12073] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12077] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12081] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12085] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12089] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [12093] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12097] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12101] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12105] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [12109] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12113] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12117] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12121] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12125] "no inflammation" "no inflammation" NA                NA               
#> [12129] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12133] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12137] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12141] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12145] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12149] "no inflammation" NA                "inflammation"    "inflammation"   
#> [12153] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12157] "inflammation"    "no inflammation" NA                "inflammation"   
#> [12161] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12165] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12169] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12173] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12177] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12181] NA                "no inflammation" "no inflammation" NA               
#> [12185] "no inflammation" "inflammation"    "no inflammation" NA               
#> [12189] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12193] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12197] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12201] NA                NA                "no inflammation" "no inflammation"
#> [12205] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12209] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12213] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12217] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12221] "no inflammation" NA                NA                "no inflammation"
#> [12225] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12229] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12233] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12237] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12241] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12245] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12249] "no inflammation" NA                "inflammation"    "no inflammation"
#> [12253] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12257] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12261] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12265] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12269] NA                "inflammation"    "no inflammation" "no inflammation"
#> [12273] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12277] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12281] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12285] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12289] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12293] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12297] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12301] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12305] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [12309] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12313] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12317] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12321] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12325] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12329] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12333] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12337] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12341] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12349] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12353] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12357] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12361] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12365] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [12369] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [12373] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12377] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12381] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12385] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12389] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12393] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12397] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12401] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [12405] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [12409] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [12413] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12417] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12421] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12425] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12429] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [12433] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [12437] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12441] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [12445] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12449] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12453] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12457] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12461] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12465] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12469] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12473] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12477] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [12481] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12485] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12489] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12493] "no inflammation" "no inflammation" "no inflammation" NA               
#> [12497] NA                "no inflammation" NA                "no inflammation"
#> [12501] "inflammation"    NA                "no inflammation" "inflammation"   
#> [12505] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12509] "no inflammation" "inflammation"    "inflammation"    NA               
#> [12513] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12517] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12521] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12525] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12533] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12537] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12541] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [12545] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12549] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12553] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12557] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12561] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [12565] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12569] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12577] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12581] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12585] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12589] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12593] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12597] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12601] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12605] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12609] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12613] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12617] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [12621] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12625] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12629] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12633] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12637] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12641] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12645] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [12649] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [12653] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12657] "no inflammation" "no inflammation" "no inflammation" NA               
#> [12661] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12665] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [12669] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12673] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12677] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [12681] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [12685] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12689] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [12693] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12697] "no inflammation" "inflammation"    NA                "no inflammation"
#> [12701] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [12705] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [12709] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12713] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12717] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12721] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12725] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12729] "no inflammation" NA                "no inflammation" NA               
#> [12733] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12737] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [12741] "inflammation"    NA                "inflammation"    "inflammation"   
#> [12745] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12749] "inflammation"    NA                "inflammation"    "inflammation"   
#> [12753] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [12757] "inflammation"    NA                "inflammation"    "inflammation"   
#> [12761] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12765] NA                "inflammation"    "inflammation"    "inflammation"   
#> [12769] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [12773] "inflammation"    NA                "no inflammation" "inflammation"   
#> [12777] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12781] NA                "inflammation"    "no inflammation" NA               
#> [12785] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12789] "no inflammation" "inflammation"    NA                "no inflammation"
#> [12793] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12797] "no inflammation" NA                "inflammation"    "no inflammation"
#> [12801] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12805] NA                "no inflammation" NA                "no inflammation"
#> [12809] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12813] "no inflammation" NA                "no inflammation" NA               
#> [12817] "inflammation"    "no inflammation" "no inflammation" NA               
#> [12821] "no inflammation" "no inflammation" "no inflammation" NA               
#> [12825] "inflammation"    "inflammation"    NA                "no inflammation"
#> [12829] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12833] NA                "no inflammation" "no inflammation" NA               
#> [12837] NA                "no inflammation" NA                "inflammation"   
#> [12841] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12845] "no inflammation" "inflammation"    "no inflammation" NA               
#> [12849] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12853] NA                "inflammation"    "no inflammation" "inflammation"   
#> [12857] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [12861] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12865] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12869] "inflammation"    NA                NA                "no inflammation"
#> [12873] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12877] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12881] NA                "no inflammation" "no inflammation" NA               
#> [12885] "no inflammation" NA                NA                "no inflammation"
#> [12889] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [12893] NA                "no inflammation" NA                "inflammation"   
#> [12897] "no inflammation" NA                "no inflammation" "inflammation"   
#> [12901] "no inflammation" NA                "no inflammation" "no inflammation"
#> [12905] NA                "inflammation"    NA                "no inflammation"
#> [12909] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12913] "no inflammation" "no inflammation" "inflammation"    NA               
#> [12917] NA                NA                "no inflammation" "no inflammation"
#> [12921] "no inflammation" NA                "no inflammation" NA               
#> [12925] "no inflammation" NA                "no inflammation" NA               
#> [12929] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12933] "no inflammation" NA                NA                NA               
#> [12937] NA                NA                NA                "no inflammation"
#> [12941] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12945] "no inflammation" NA                NA                "no inflammation"
#> [12949] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12953] "no inflammation" "no inflammation" NA                NA               
#> [12957] NA                "inflammation"    "inflammation"    "no inflammation"
#> [12961] NA                NA                NA                NA               
#> [12965] "no inflammation" "no inflammation" NA                "inflammation"   
#> [12969] NA                "no inflammation" "no inflammation" NA               
#> [12973] "inflammation"    NA                "no inflammation" NA               
#> [12977] NA                "no inflammation" "no inflammation" NA               
#> [12981] "no inflammation" "no inflammation" NA                NA               
#> [12985] NA                "no inflammation" "no inflammation" "no inflammation"
#> [12989] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [12993] "no inflammation" "no inflammation" NA                "no inflammation"
#> [12997] NA                "inflammation"    "no inflammation" NA               
#> [13001] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13005] NA                "no inflammation" "no inflammation" NA               
#> [13009] NA                NA                NA                NA               
#> [13013] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13017] "inflammation"    "no inflammation" NA                "no inflammation"
#> [13021] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [13025] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13029] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13033] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13037] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13041] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13045] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13049] NA                NA                NA                NA               
#> [13053] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13057] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13061] "no inflammation" "no inflammation" NA                NA               
#> [13065] "no inflammation" NA                NA                NA               
#> [13069] "no inflammation" NA                NA                NA               
#> [13073] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13077] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13081] NA                NA                "no inflammation" "no inflammation"
#> [13085] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [13089] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13093] "inflammation"    "no inflammation" NA                "no inflammation"
#> [13097] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13101] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13105] NA                "no inflammation" NA                "no inflammation"
#> [13109] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13113] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13117] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13121] "no inflammation" "no inflammation" NA                "inflammation"   
#> [13125] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [13129] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13133] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13137] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13141] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13145] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13149] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13153] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13157] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13161] "no inflammation" "inflammation"    NA                "no inflammation"
#> [13165] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13169] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13173] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13177] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13181] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13185] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13189] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13193] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13197] "no inflammation" NA                "no inflammation" NA               
#> [13201] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13205] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13209] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13213] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13217] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13221] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13225] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13229] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13233] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13237] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13241] NA                NA                "no inflammation" NA               
#> [13245] "no inflammation" NA                NA                NA               
#> [13249] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13253] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13257] NA                "no inflammation" "no inflammation" NA               
#> [13261] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13265] "inflammation"    "no inflammation" NA                NA               
#> [13269] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13273] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13277] NA                "inflammation"    NA                "no inflammation"
#> [13281] NA                NA                "no inflammation" "no inflammation"
#> [13285] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13289] NA                "inflammation"    "no inflammation" NA               
#> [13293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13297] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13301] NA                NA                "no inflammation" NA               
#> [13305] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13309] NA                "no inflammation" NA                "no inflammation"
#> [13313] "no inflammation" "inflammation"    NA                "no inflammation"
#> [13317] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13321] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13325] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13329] "no inflammation" "inflammation"    NA                "no inflammation"
#> [13333] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13337] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13341] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13345] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13349] "inflammation"    "no inflammation" "no inflammation" NA               
#> [13353] "inflammation"    "no inflammation" NA                "no inflammation"
#> [13357] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13361] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13365] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13369] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13373] "no inflammation" NA                "no inflammation" NA               
#> [13377] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13381] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13385] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13389] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13393] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13397] "no inflammation" "no inflammation" NA                NA               
#> [13401] "no inflammation" NA                "inflammation"    "no inflammation"
#> [13405] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13409] "no inflammation" NA                NA                "no inflammation"
#> [13413] NA                "inflammation"    "no inflammation" "no inflammation"
#> [13417] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13421] NA                NA                "inflammation"    "no inflammation"
#> [13425] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13429] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13433] NA                "inflammation"    "no inflammation" "no inflammation"
#> [13437] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13441] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13445] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [13449] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13453] NA                "inflammation"    "no inflammation" "no inflammation"
#> [13457] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13461] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13465] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13469] NA                "no inflammation" "inflammation"    "no inflammation"
#> [13473] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13477] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13481] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13485] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13489] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [13493] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [13497] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13501] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [13505] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13509] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13513] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13517] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13521] "no inflammation" NA                "no inflammation" NA               
#> [13525] "no inflammation" "no inflammation" NA                "inflammation"   
#> [13529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13533] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13537] "inflammation"    "no inflammation" "no inflammation" NA               
#> [13541] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13545] NA                "no inflammation" "no inflammation" NA               
#> [13549] "no inflammation" NA                "no inflammation" "inflammation"   
#> [13553] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13557] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13561] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13565] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13569] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13573] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [13577] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13581] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13585] NA                "no inflammation" "no inflammation" "no inflammation"
#> [13589] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13593] NA                "inflammation"    NA                NA               
#> [13597] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [13601] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [13605] NA                NA                "no inflammation" NA               
#> [13609] "inflammation"    NA                "inflammation"    "inflammation"   
#> [13613] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [13617] NA                "no inflammation" NA                "no inflammation"
#> [13621] "no inflammation" "inflammation"    NA                "no inflammation"
#> [13625] "no inflammation" "inflammation"    NA                NA               
#> [13629] "inflammation"    "inflammation"    "inflammation"    NA               
#> [13633] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13637] "inflammation"    "inflammation"    "no inflammation" NA               
#> [13641] NA                "inflammation"    NA                "no inflammation"
#> [13645] "inflammation"    "no inflammation" "no inflammation" NA               
#> [13649] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [13653] NA                "no inflammation" NA                "no inflammation"
#> [13657] NA                "inflammation"    NA                "no inflammation"
#> [13661] "no inflammation" "inflammation"    NA                NA               
#> [13665] "no inflammation" NA                "no inflammation" NA               
#> [13669] "inflammation"    "no inflammation" "inflammation"    NA               
#> [13673] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [13677] "no inflammation" NA                "no inflammation" "no inflammation"
#> [13681] NA                NA                NA                "no inflammation"
#> [13685] NA                NA                "inflammation"    NA               
#> [13689] NA                NA                "no inflammation" NA               
#> [13693] "no inflammation" "inflammation"    "no inflammation" NA               
#> [13697] NA                "inflammation"    "inflammation"    "inflammation"   
#> [13701] NA                "inflammation"    NA                NA               
#> [13705] "inflammation"    "inflammation"    NA                "no inflammation"
#> [13709] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13713] "inflammation"    "no inflammation" NA                "no inflammation"
#> [13717] NA                "inflammation"    NA                "inflammation"   
#> [13721] "inflammation"    NA                NA                "inflammation"   
#> [13725] "inflammation"    NA                NA                "no inflammation"
#> [13729] NA                "inflammation"    NA                "no inflammation"
#> [13733] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [13737] "inflammation"    NA                "inflammation"    "inflammation"   
#> [13741] "inflammation"    NA                "no inflammation" "inflammation"   
#> [13745] "no inflammation" "no inflammation" "no inflammation" NA               
#> [13749] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [13753] NA                NA                "inflammation"    "inflammation"   
#> [13757] NA                NA                "no inflammation" NA               
#> [13761] NA                NA                "inflammation"    NA               
#> [13765] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [13769] NA                NA                "inflammation"    "no inflammation"
#> [13773] "no inflammation" "inflammation"    NA                NA               
#> [13777] NA                NA                NA                NA               
#> [13781] "inflammation"    NA                "no inflammation" NA               
#> [13785] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [13789] "inflammation"    NA                NA                "no inflammation"
#> [13793] NA                NA                "inflammation"    NA               
#> [13797] "no inflammation" "no inflammation" NA                NA               
#> [13801] "inflammation"    "inflammation"    NA                "no inflammation"
#> [13805] "inflammation"    "inflammation"    "inflammation"    NA               
#> [13809] NA                NA                "inflammation"    "inflammation"   
#> [13813] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [13817] "no inflammation" "inflammation"    "no inflammation" NA               
#> [13821] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [13825] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [13829] "no inflammation" "inflammation"    "no inflammation" NA               
#> [13833] NA                NA                "inflammation"    NA               
#> [13837] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13841] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [13845] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [13849] "no inflammation" "inflammation"    "no inflammation" NA               
#> [13853] NA                NA                "no inflammation" "no inflammation"
#> [13857] NA                NA                "no inflammation" "inflammation"   
#> [13861] NA                "no inflammation" "inflammation"    NA               
#> [13865] NA                "no inflammation" "inflammation"    "inflammation"   
#> [13869] "inflammation"    NA                "inflammation"    "no inflammation"
#> [13873] NA                "inflammation"    "inflammation"    "inflammation"   
#> [13877] "no inflammation" "no inflammation" "inflammation"    NA               
#> [13881] NA                NA                "no inflammation" NA               
#> [13885] "no inflammation" "no inflammation" "inflammation"    NA               
#> [13889] NA                "no inflammation" "inflammation"    "no inflammation"
#> [13893] "no inflammation" "inflammation"    NA                NA               
#> [13897] NA                "no inflammation" "inflammation"    "inflammation"   
#> [13901] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [13905] "inflammation"    NA                "no inflammation" "no inflammation"
#> [13909] "inflammation"    NA                "no inflammation" "inflammation"   
#> [13913] NA                "no inflammation" "inflammation"    "no inflammation"
#> [13917] "no inflammation" NA                NA                "inflammation"   
#> [13921] NA                "inflammation"    "no inflammation" "no inflammation"
#> [13925] NA                NA                "no inflammation" NA               
#> [13929] NA                "no inflammation" NA                "no inflammation"
#> [13933] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [13937] NA                NA                NA                "no inflammation"
#> [13941] "inflammation"    NA                "no inflammation" NA               
#> [13945] "no inflammation" "no inflammation" NA                "no inflammation"
#> [13949] NA                "no inflammation" "no inflammation" NA               
#> [13953] NA                "inflammation"    "no inflammation" "inflammation"   
#> [13957] NA                NA                "inflammation"    "inflammation"   
#> [13961] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [13965] NA                "inflammation"    "inflammation"    "inflammation"   
#> [13969] "no inflammation" "inflammation"    NA                "no inflammation"
#> [13973] "no inflammation" NA                NA                "inflammation"   
#> [13977] "no inflammation" "no inflammation" NA                NA               
#> [13981] "no inflammation" "inflammation"    NA                NA               
#> [13985] "no inflammation" NA                "inflammation"    "no inflammation"
#> [13989] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [13993] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [13997] "no inflammation" "inflammation"    "inflammation"    NA               
#> [14001] "inflammation"    NA                "no inflammation" "no inflammation"
#> [14005] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14009] "no inflammation" "inflammation"    NA                NA               
#> [14013] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [14017] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [14021] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14025] NA                NA                "inflammation"    "inflammation"   
#> [14029] "no inflammation" "inflammation"    NA                "inflammation"   
#> [14033] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [14037] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14041] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14045] NA                NA                "inflammation"    "inflammation"   
#> [14049] "inflammation"    "inflammation"    "no inflammation" NA               
#> [14053] NA                NA                "no inflammation" "inflammation"   
#> [14057] "no inflammation" NA                "inflammation"    "inflammation"   
#> [14061] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14065] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [14069] NA                "inflammation"    "inflammation"    "inflammation"   
#> [14073] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [14077] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14081] NA                NA                NA                NA               
#> [14085] "no inflammation" NA                "inflammation"    "inflammation"   
#> [14089] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [14093] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [14097] "no inflammation" "no inflammation" NA                NA               
#> [14101] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [14105] "inflammation"    "inflammation"    NA                "no inflammation"
#> [14109] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14113] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [14117] "inflammation"    NA                "no inflammation" "no inflammation"
#> [14121] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14125] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [14129] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [14133] "no inflammation" "inflammation"    NA                "no inflammation"
#> [14137] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [14141] "no inflammation" "no inflammation" "no inflammation" NA               
#> [14145] "inflammation"    "no inflammation" "no inflammation" NA               
#> [14149] NA                NA                NA                NA               
#> [14153] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14157] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14161] "inflammation"    "no inflammation" "inflammation"    NA               
#> [14165] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14169] NA                NA                "no inflammation" "no inflammation"
#> [14173] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14177] "inflammation"    "no inflammation" NA                "inflammation"   
#> [14181] "inflammation"    "no inflammation" NA                "inflammation"   
#> [14185] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14189] "no inflammation" "inflammation"    "no inflammation" NA               
#> [14193] NA                NA                NA                "inflammation"   
#> [14197] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14201] "inflammation"    "inflammation"    "inflammation"    NA               
#> [14205] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14209] "inflammation"    "inflammation"    "no inflammation" NA               
#> [14213] NA                "inflammation"    "inflammation"    "inflammation"   
#> [14217] "no inflammation" "inflammation"    NA                NA               
#> [14221] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14225] "no inflammation" "no inflammation" NA                "inflammation"   
#> [14229] NA                NA                "inflammation"    "no inflammation"
#> [14233] "inflammation"    NA                NA                "no inflammation"
#> [14237] "inflammation"    NA                "inflammation"    "inflammation"   
#> [14241] "inflammation"    NA                "inflammation"    "no inflammation"
#> [14245] "no inflammation" "inflammation"    NA                "no inflammation"
#> [14249] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14253] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14257] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14261] NA                "no inflammation" "inflammation"    NA               
#> [14265] "inflammation"    "no inflammation" "no inflammation" NA               
#> [14269] "inflammation"    "no inflammation" NA                NA               
#> [14273] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14277] "inflammation"    "no inflammation" NA                "inflammation"   
#> [14281] NA                "inflammation"    "no inflammation" "inflammation"   
#> [14285] NA                NA                "no inflammation" "no inflammation"
#> [14289] "no inflammation" NA                "inflammation"    "inflammation"   
#> [14293] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14297] NA                NA                NA                "no inflammation"
#> [14301] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [14305] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [14309] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [14313] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14317] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14321] NA                NA                "inflammation"    "inflammation"   
#> [14325] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [14329] "inflammation"    "inflammation"    NA                "inflammation"   
#> [14333] NA                NA                "inflammation"    "inflammation"   
#> [14337] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [14341] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14349] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14353] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14357] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [14361] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [14365] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14369] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [14373] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14377] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14381] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14385] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14389] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14393] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14397] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14401] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14405] "no inflammation" "no inflammation" "no inflammation" NA               
#> [14409] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14413] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14417] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14421] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14425] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14429] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14433] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14437] "no inflammation" "inflammation"    "no inflammation" NA               
#> [14441] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14445] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14449] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14453] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14457] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14461] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14465] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14469] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14473] "no inflammation" NA                NA                "no inflammation"
#> [14477] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14481] NA                "no inflammation" NA                "inflammation"   
#> [14485] "no inflammation" NA                NA                NA               
#> [14489] "no inflammation" "inflammation"    NA                NA               
#> [14493] NA                NA                "no inflammation" NA               
#> [14497] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14501] NA                "no inflammation" NA                "no inflammation"
#> [14505] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [14509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14513] NA                NA                "no inflammation" "no inflammation"
#> [14517] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14521] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14525] "no inflammation" "no inflammation" "no inflammation" NA               
#> [14529] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14533] "no inflammation" NA                "inflammation"    "no inflammation"
#> [14537] "no inflammation" "no inflammation" NA                NA               
#> [14541] NA                NA                NA                "no inflammation"
#> [14545] NA                NA                NA                NA               
#> [14549] NA                "no inflammation" NA                "inflammation"   
#> [14553] NA                "no inflammation" NA                "no inflammation"
#> [14557] "inflammation"    NA                NA                NA               
#> [14561] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14565] "no inflammation" NA                NA                "inflammation"   
#> [14569] NA                "no inflammation" "no inflammation" NA               
#> [14573] NA                NA                NA                NA               
#> [14577] NA                "no inflammation" "no inflammation" NA               
#> [14581] "no inflammation" "inflammation"    "no inflammation" NA               
#> [14585] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14589] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14593] NA                NA                "no inflammation" "no inflammation"
#> [14597] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14601] NA                "no inflammation" "no inflammation" NA               
#> [14605] "no inflammation" NA                NA                "inflammation"   
#> [14609] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14613] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14617] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14621] "no inflammation" NA                "no inflammation" NA               
#> [14625] "no inflammation" NA                NA                "no inflammation"
#> [14629] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [14633] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14637] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14641] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [14645] "no inflammation" "no inflammation" "no inflammation" NA               
#> [14649] "inflammation"    NA                "no inflammation" "no inflammation"
#> [14653] NA                NA                "no inflammation" NA               
#> [14657] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14661] "no inflammation" NA                "no inflammation" NA               
#> [14665] NA                NA                "no inflammation" NA               
#> [14669] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14673] NA                "no inflammation" "inflammation"    "no inflammation"
#> [14677] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14681] NA                "no inflammation" "no inflammation" NA               
#> [14685] "no inflammation" NA                NA                "no inflammation"
#> [14689] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14693] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14697] "no inflammation" NA                NA                "no inflammation"
#> [14701] "no inflammation" "no inflammation" NA                NA               
#> [14705] "no inflammation" NA                "inflammation"    "no inflammation"
#> [14709] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14713] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14717] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14721] "inflammation"    "no inflammation" NA                NA               
#> [14725] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14729] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14733] "no inflammation" "inflammation"    "no inflammation" NA               
#> [14737] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14741] NA                "no inflammation" NA                NA               
#> [14745] "no inflammation" NA                "inflammation"    NA               
#> [14749] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14753] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14757] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14761] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14765] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14769] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14773] NA                NA                "inflammation"    "no inflammation"
#> [14777] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14781] "no inflammation" NA                "no inflammation" "no inflammation"
#> [14785] "no inflammation" "inflammation"    "no inflammation" NA               
#> [14789] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14793] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [14797] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14801] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14805] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14809] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [14813] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14817] NA                NA                "inflammation"    "inflammation"   
#> [14821] "inflammation"    NA                "inflammation"    "inflammation"   
#> [14825] "no inflammation" "inflammation"    NA                NA               
#> [14829] NA                NA                "no inflammation" NA               
#> [14833] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14837] NA                "inflammation"    "no inflammation" "no inflammation"
#> [14841] NA                "no inflammation" NA                NA               
#> [14845] NA                NA                NA                NA               
#> [14849] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [14853] "inflammation"    "inflammation"    "inflammation"    NA               
#> [14857] NA                "no inflammation" NA                "no inflammation"
#> [14861] NA                "no inflammation" "inflammation"    "inflammation"   
#> [14865] "inflammation"    "no inflammation" NA                NA               
#> [14869] NA                NA                "no inflammation" "no inflammation"
#> [14873] NA                NA                "no inflammation" NA               
#> [14877] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14881] "no inflammation" "no inflammation" "no inflammation" NA               
#> [14885] "no inflammation" NA                NA                "no inflammation"
#> [14889] NA                "no inflammation" NA                "no inflammation"
#> [14893] NA                "no inflammation" "no inflammation" NA               
#> [14897] NA                NA                "no inflammation" "no inflammation"
#> [14901] "no inflammation" "no inflammation" NA                "no inflammation"
#> [14905] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14909] NA                "no inflammation" NA                NA               
#> [14913] NA                "inflammation"    "inflammation"    "no inflammation"
#> [14917] NA                "no inflammation" "no inflammation" NA               
#> [14921] "no inflammation" NA                NA                "no inflammation"
#> [14925] NA                "no inflammation" "no inflammation" "no inflammation"
#> [14929] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14933] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14937] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [14941] NA                NA                NA                "no inflammation"
#> [14945] NA                NA                "inflammation"    "no inflammation"
#> [14949] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [14953] NA                "no inflammation" NA                "no inflammation"
#> [14957] NA                "no inflammation" NA                "no inflammation"
#> [14961] "inflammation"    "no inflammation" NA                "no inflammation"
#> [14965] "no inflammation" "inflammation"    NA                "inflammation"   
#> [14969] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14973] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14977] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [14981] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [14985] "no inflammation" NA                "inflammation"    NA               
#> [14989] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [14993] NA                NA                "no inflammation" "no inflammation"
#> [14997] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15001] NA                "inflammation"    "no inflammation" "no inflammation"
#> [15005] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15009] "no inflammation" "inflammation"    NA                NA               
#> [15013] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15017] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15021] "inflammation"    NA                NA                NA               
#> [15025] "inflammation"    NA                "no inflammation" "no inflammation"
#> [15029] NA                "inflammation"    "no inflammation" "inflammation"   
#> [15033] NA                NA                "no inflammation" "no inflammation"
#> [15037] NA                NA                NA                NA               
#> [15041] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15045] NA                NA                NA                NA               
#> [15049] "inflammation"    NA                NA                "no inflammation"
#> [15053] NA                "no inflammation" NA                NA               
#> [15057] NA                NA                NA                "no inflammation"
#> [15061] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15065] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [15069] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15073] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15077] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15081] "inflammation"    NA                NA                "inflammation"   
#> [15085] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15089] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15093] "no inflammation" "inflammation"    "inflammation"    NA               
#> [15097] "inflammation"    "no inflammation" NA                "no inflammation"
#> [15101] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15105] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15109] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [15113] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15117] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15121] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15125] "no inflammation" "no inflammation" NA                NA               
#> [15129] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [15133] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15137] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [15141] "inflammation"    NA                "no inflammation" "inflammation"   
#> [15145] NA                "no inflammation" "no inflammation" "inflammation"   
#> [15149] "inflammation"    NA                NA                "no inflammation"
#> [15153] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [15157] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15161] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15165] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15169] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [15173] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [15177] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15181] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15185] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15189] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15193] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15197] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15201] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15205] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15209] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15213] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15217] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15221] "no inflammation" "inflammation"    "no inflammation" NA               
#> [15225] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15229] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15233] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15237] NA                NA                NA                "no inflammation"
#> [15241] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15245] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15249] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15253] "inflammation"    NA                "no inflammation" "no inflammation"
#> [15257] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15261] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15265] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [15269] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15273] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15277] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [15281] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15285] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15289] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15297] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15301] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15305] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15309] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15313] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [15317] "inflammation"    "no inflammation" NA                NA               
#> [15321] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15325] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [15329] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15333] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [15337] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [15341] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [15345] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [15349] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [15353] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15357] NA                "inflammation"    "no inflammation" "inflammation"   
#> [15361] "no inflammation" "no inflammation" NA                "inflammation"   
#> [15365] "no inflammation" "no inflammation" NA                "inflammation"   
#> [15369] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15373] NA                "no inflammation" NA                "no inflammation"
#> [15377] "inflammation"    "no inflammation" "inflammation"    NA               
#> [15381] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [15385] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15389] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [15393] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [15397] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15401] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15405] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15409] "no inflammation" NA                NA                "no inflammation"
#> [15413] NA                NA                "no inflammation" "no inflammation"
#> [15417] "no inflammation" NA                "inflammation"    "no inflammation"
#> [15421] "no inflammation" "inflammation"    NA                NA               
#> [15425] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15429] "no inflammation" NA                "inflammation"    NA               
#> [15433] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15437] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [15441] "no inflammation" "inflammation"    "inflammation"    NA               
#> [15445] NA                "no inflammation" "no inflammation" NA               
#> [15449] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15453] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15457] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15461] NA                "inflammation"    "no inflammation" "no inflammation"
#> [15465] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15469] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15473] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15477] "no inflammation" NA                NA                "no inflammation"
#> [15481] "no inflammation" NA                "no inflammation" "inflammation"   
#> [15485] "no inflammation" NA                "inflammation"    NA               
#> [15489] "no inflammation" NA                NA                "no inflammation"
#> [15493] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15497] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15501] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15505] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15513] "no inflammation" "no inflammation" NA                NA               
#> [15517] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15521] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15525] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15529] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [15533] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15537] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15541] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15545] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15549] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15553] "no inflammation" NA                NA                "no inflammation"
#> [15557] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15561] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15565] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [15569] NA                "no inflammation" "inflammation"    NA               
#> [15573] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15577] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15581] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15585] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15589] NA                NA                "no inflammation" "no inflammation"
#> [15593] "no inflammation" NA                "no inflammation" "no inflammation"
#> [15597] "no inflammation" NA                NA                NA               
#> [15601] NA                "no inflammation" NA                "no inflammation"
#> [15605] "no inflammation" "no inflammation" "inflammation"    NA               
#> [15609] "no inflammation" "no inflammation" NA                "inflammation"   
#> [15613] NA                "no inflammation" "no inflammation" NA               
#> [15617] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15621] NA                "inflammation"    NA                "no inflammation"
#> [15625] "no inflammation" NA                NA                NA               
#> [15629] "no inflammation" "inflammation"    "no inflammation" NA               
#> [15633] "inflammation"    "inflammation"    NA                "no inflammation"
#> [15637] "inflammation"    NA                "inflammation"    "no inflammation"
#> [15641] NA                NA                NA                NA               
#> [15645] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [15649] NA                "no inflammation" "inflammation"    "inflammation"   
#> [15653] "inflammation"    NA                "inflammation"    "no inflammation"
#> [15657] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [15661] "no inflammation" NA                NA                "no inflammation"
#> [15665] NA                "inflammation"    "inflammation"    "no inflammation"
#> [15669] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [15673] NA                "no inflammation" "no inflammation" NA               
#> [15677] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15681] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15685] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15689] "no inflammation" "inflammation"    NA                NA               
#> [15693] "inflammation"    "no inflammation" "inflammation"    NA               
#> [15697] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [15701] "no inflammation" "inflammation"    NA                NA               
#> [15705] "no inflammation" NA                "no inflammation" "inflammation"   
#> [15709] "no inflammation" NA                "inflammation"    NA               
#> [15713] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15717] NA                "inflammation"    "no inflammation" "no inflammation"
#> [15721] NA                "inflammation"    NA                "no inflammation"
#> [15725] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15729] "no inflammation" "no inflammation" NA                NA               
#> [15733] "inflammation"    "no inflammation" "inflammation"    NA               
#> [15737] "no inflammation" NA                NA                "inflammation"   
#> [15741] "inflammation"    "no inflammation" NA                NA               
#> [15745] "no inflammation" "no inflammation" NA                "no inflammation"
#> [15749] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [15753] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15757] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15761] "inflammation"    NA                NA                NA               
#> [15765] NA                "no inflammation" NA                "no inflammation"
#> [15769] "inflammation"    "inflammation"    NA                "no inflammation"
#> [15773] NA                "inflammation"    NA                "no inflammation"
#> [15777] "inflammation"    NA                "inflammation"    NA               
#> [15781] "inflammation"    "no inflammation" NA                NA               
#> [15785] NA                NA                "inflammation"    "inflammation"   
#> [15789] "no inflammation" NA                "no inflammation" "inflammation"   
#> [15793] NA                "inflammation"    "inflammation"    "inflammation"   
#> [15797] NA                "no inflammation" "no inflammation" "inflammation"   
#> [15801] NA                "no inflammation" "inflammation"    NA               
#> [15805] "no inflammation" NA                NA                "no inflammation"
#> [15809] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15813] "no inflammation" NA                "no inflammation" NA               
#> [15817] "inflammation"    "inflammation"    "inflammation"    NA               
#> [15821] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [15825] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [15829] NA                "inflammation"    "inflammation"    "inflammation"   
#> [15833] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [15837] "no inflammation" "inflammation"    NA                "no inflammation"
#> [15841] NA                "no inflammation" "inflammation"    "no inflammation"
#> [15845] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15849] NA                "inflammation"    "no inflammation" "inflammation"   
#> [15853] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [15857] NA                "no inflammation" "no inflammation" "no inflammation"
#> [15861] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15865] "inflammation"    "inflammation"    NA                NA               
#> [15869] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15873] NA                NA                "inflammation"    NA               
#> [15877] NA                "no inflammation" "inflammation"    "no inflammation"
#> [15881] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [15885] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [15889] "no inflammation" "no inflammation" "no inflammation" NA               
#> [15893] NA                "no inflammation" "inflammation"    NA               
#> [15897] "no inflammation" NA                "inflammation"    NA               
#> [15901] "inflammation"    "inflammation"    NA                NA               
#> [15905] "inflammation"    NA                NA                "no inflammation"
#> [15909] NA                "inflammation"    NA                "inflammation"   
#> [15913] "inflammation"    NA                "no inflammation" NA               
#> [15917] NA                "inflammation"    NA                "no inflammation"
#> [15921] "inflammation"    NA                "inflammation"    "inflammation"   
#> [15925] "inflammation"    NA                "inflammation"    "no inflammation"
#> [15929] "inflammation"    NA                "no inflammation" "no inflammation"
#> [15933] NA                "inflammation"    NA                "inflammation"   
#> [15937] "inflammation"    NA                NA                "inflammation"   
#> [15941] "inflammation"    "no inflammation" "no inflammation" NA               
#> [15945] "inflammation"    "inflammation"    NA                NA               
#> [15949] "no inflammation" NA                "no inflammation" NA               
#> [15953] NA                NA                NA                NA               
#> [15957] NA                "inflammation"    NA                NA               
#> [15961] NA                NA                "inflammation"    "inflammation"   
#> [15965] NA                "inflammation"    NA                "inflammation"   
#> [15969] NA                "inflammation"    "inflammation"    "no inflammation"
#> [15973] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [15977] "inflammation"    NA                "inflammation"    "no inflammation"
#> [15981] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [15985] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [15989] "no inflammation" NA                "inflammation"    NA               
#> [15993] "no inflammation" "no inflammation" NA                NA               
#> [15997] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16001] "no inflammation" "inflammation"    "no inflammation" NA               
#> [16005] "no inflammation" NA                "inflammation"    "no inflammation"
#> [16009] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16013] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16017] "inflammation"    "inflammation"    NA                "inflammation"   
#> [16021] "no inflammation" NA                "inflammation"    "inflammation"   
#> [16025] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16029] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16033] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16037] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [16041] "inflammation"    "no inflammation" "inflammation"    NA               
#> [16045] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [16049] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16053] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [16057] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16061] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16065] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16069] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16073] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [16077] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16081] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16085] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16089] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16093] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16097] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16101] "inflammation"    "no inflammation" "no inflammation" NA               
#> [16105] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16109] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16113] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16117] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16121] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16125] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16129] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16133] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16137] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16141] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16145] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16149] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16153] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16157] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16161] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16165] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16169] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16173] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16177] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16181] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16185] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16189] "no inflammation" "inflammation"    NA                "no inflammation"
#> [16193] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16197] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [16201] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16205] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [16209] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16213] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16217] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [16221] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16225] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16229] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [16233] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16237] "inflammation"    NA                "no inflammation" "no inflammation"
#> [16241] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16245] NA                "inflammation"    "no inflammation" "no inflammation"
#> [16249] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [16253] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16257] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16261] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16265] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [16269] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [16273] "no inflammation" "inflammation"    NA                "no inflammation"
#> [16277] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16281] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16285] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16289] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16293] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16297] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16301] "no inflammation" "no inflammation" "inflammation"    NA               
#> [16305] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16309] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16313] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16317] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16321] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [16325] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16329] NA                "no inflammation" "no inflammation" "inflammation"   
#> [16333] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16337] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16341] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16345] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16349] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16353] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16357] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16361] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16365] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16369] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16373] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16377] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [16381] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16385] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16389] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16393] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16397] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16401] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16405] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16409] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16413] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16417] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [16421] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [16425] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16429] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [16433] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16437] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16441] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16445] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16449] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16453] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16457] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16461] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16465] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16469] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16473] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16477] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16481] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16485] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16489] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16493] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16497] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16501] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16505] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [16509] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16513] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16517] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16521] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16525] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16529] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [16533] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16537] "no inflammation" NA                NA                "no inflammation"
#> [16541] "no inflammation" NA                "inflammation"    "inflammation"   
#> [16545] NA                "no inflammation" NA                "no inflammation"
#> [16549] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16553] NA                "inflammation"    "no inflammation" "no inflammation"
#> [16557] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16561] NA                NA                NA                "no inflammation"
#> [16565] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16569] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16573] "inflammation"    NA                NA                NA               
#> [16577] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16581] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16585] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16589] NA                NA                "no inflammation" "no inflammation"
#> [16593] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16597] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16601] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16605] NA                NA                "no inflammation" NA               
#> [16609] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16613] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16617] "no inflammation" NA                NA                NA               
#> [16621] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [16625] "inflammation"    NA                "inflammation"    NA               
#> [16629] NA                "no inflammation" "no inflammation" NA               
#> [16633] "no inflammation" NA                "inflammation"    "no inflammation"
#> [16637] NA                "no inflammation" "no inflammation" "inflammation"   
#> [16641] "inflammation"    "inflammation"    NA                "no inflammation"
#> [16645] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16649] "no inflammation" "no inflammation" "no inflammation" NA               
#> [16653] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16657] NA                "no inflammation" "inflammation"    "no inflammation"
#> [16661] "no inflammation" "no inflammation" NA                NA               
#> [16665] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16669] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16673] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16677] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16681] NA                "no inflammation" "no inflammation" "inflammation"   
#> [16685] "no inflammation" NA                "no inflammation" NA               
#> [16689] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16693] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16697] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16701] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16705] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16709] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16713] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16717] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16721] "no inflammation" NA                NA                "no inflammation"
#> [16725] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16729] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16733] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16737] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16741] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16745] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16749] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16753] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16757] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16761] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [16765] "inflammation"    NA                "no inflammation" "no inflammation"
#> [16769] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16773] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16777] "inflammation"    "no inflammation" NA                NA               
#> [16781] "no inflammation" NA                NA                "no inflammation"
#> [16785] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16789] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16793] NA                "no inflammation" NA                "no inflammation"
#> [16797] NA                "inflammation"    "no inflammation" "no inflammation"
#> [16801] "no inflammation" NA                "inflammation"    "no inflammation"
#> [16805] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16809] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16813] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16817] NA                "no inflammation" "no inflammation" "no inflammation"
#> [16821] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16825] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16829] "no inflammation" "no inflammation" "no inflammation" NA               
#> [16833] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16837] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16841] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16845] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16849] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16853] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16857] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16861] "no inflammation" "no inflammation" NA                "no inflammation"
#> [16865] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16869] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16873] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16877] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16881] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16885] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16889] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [16893] "inflammation"    "no inflammation" NA                "no inflammation"
#> [16897] "inflammation"    NA                "no inflammation" NA               
#> [16901] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16905] NA                "inflammation"    "inflammation"    "no inflammation"
#> [16909] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [16913] "inflammation"    "inflammation"    "no inflammation" NA               
#> [16917] "no inflammation" "inflammation"    "no inflammation" NA               
#> [16921] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [16925] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16929] "inflammation"    "inflammation"    NA                "inflammation"   
#> [16933] "inflammation"    "no inflammation" NA                NA               
#> [16937] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [16941] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [16945] NA                NA                "inflammation"    "inflammation"   
#> [16949] NA                NA                NA                "no inflammation"
#> [16953] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [16957] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16961] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16965] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [16969] "no inflammation" "no inflammation" "no inflammation" NA               
#> [16973] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [16977] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [16981] NA                "inflammation"    "no inflammation" "no inflammation"
#> [16985] "no inflammation" NA                "no inflammation" "no inflammation"
#> [16989] "inflammation"    "no inflammation" "no inflammation" NA               
#> [16993] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [16997] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17001] NA                "no inflammation" NA                "no inflammation"
#> [17005] "no inflammation" NA                NA                "inflammation"   
#> [17009] NA                "no inflammation" "inflammation"    NA               
#> [17013] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [17017] NA                "no inflammation" "no inflammation" NA               
#> [17021] "no inflammation" NA                NA                NA               
#> [17025] "no inflammation" "no inflammation" NA                NA               
#> [17029] NA                NA                "inflammation"    NA               
#> [17033] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17037] NA                "inflammation"    "no inflammation" "inflammation"   
#> [17041] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17045] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [17049] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17053] "no inflammation" "no inflammation" "inflammation"    NA               
#> [17057] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17061] NA                "no inflammation" NA                NA               
#> [17065] "inflammation"    NA                NA                "no inflammation"
#> [17069] "inflammation"    NA                "no inflammation" NA               
#> [17073] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17077] "inflammation"    NA                "no inflammation" NA               
#> [17081] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17085] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17089] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17093] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [17097] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17101] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17105] NA                NA                "no inflammation" NA               
#> [17109] NA                "no inflammation" NA                "no inflammation"
#> [17113] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17117] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17121] "no inflammation" NA                "inflammation"    "no inflammation"
#> [17125] "inflammation"    "no inflammation" NA                NA               
#> [17129] NA                "no inflammation" "inflammation"    "no inflammation"
#> [17133] NA                NA                "no inflammation" "inflammation"   
#> [17137] "no inflammation" NA                "inflammation"    "inflammation"   
#> [17141] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17145] "no inflammation" NA                NA                NA               
#> [17149] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17153] "no inflammation" NA                "no inflammation" "no inflammation"
#> [17157] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17161] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17165] NA                "inflammation"    "no inflammation" "no inflammation"
#> [17169] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17173] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17177] NA                "inflammation"    "inflammation"    "inflammation"   
#> [17181] NA                "inflammation"    "no inflammation" "no inflammation"
#> [17185] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17189] "inflammation"    NA                NA                "inflammation"   
#> [17193] "no inflammation" "no inflammation" NA                NA               
#> [17197] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17201] NA                "no inflammation" "no inflammation" "inflammation"   
#> [17205] "inflammation"    NA                "no inflammation" NA               
#> [17209] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17213] "no inflammation" "no inflammation" NA                "inflammation"   
#> [17217] "inflammation"    "no inflammation" NA                NA               
#> [17221] "no inflammation" "inflammation"    NA                NA               
#> [17225] NA                "no inflammation" NA                "no inflammation"
#> [17229] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17233] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17237] "no inflammation" NA                NA                NA               
#> [17241] "inflammation"    NA                NA                "no inflammation"
#> [17245] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17249] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17253] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17257] "inflammation"    NA                "no inflammation" "no inflammation"
#> [17261] "no inflammation" "inflammation"    NA                "no inflammation"
#> [17265] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17269] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17273] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17277] "inflammation"    "no inflammation" "no inflammation" NA               
#> [17281] NA                "inflammation"    "no inflammation" "no inflammation"
#> [17285] "no inflammation" "no inflammation" NA                NA               
#> [17289] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17293] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17297] "inflammation"    "no inflammation" NA                "inflammation"   
#> [17301] NA                "no inflammation" NA                "no inflammation"
#> [17305] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17309] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17313] "no inflammation" NA                "no inflammation" NA               
#> [17317] "inflammation"    "inflammation"    "no inflammation" NA               
#> [17321] NA                NA                "inflammation"    "no inflammation"
#> [17325] NA                "inflammation"    "no inflammation" "inflammation"   
#> [17329] NA                "inflammation"    NA                "no inflammation"
#> [17333] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17337] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17341] NA                "no inflammation" "inflammation"    NA               
#> [17345] NA                NA                NA                "inflammation"   
#> [17349] NA                NA                NA                "no inflammation"
#> [17353] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17357] NA                "no inflammation" NA                NA               
#> [17361] NA                NA                NA                "no inflammation"
#> [17365] "no inflammation" NA                "no inflammation" "no inflammation"
#> [17369] "no inflammation" NA                NA                "no inflammation"
#> [17373] "no inflammation" NA                "no inflammation" NA               
#> [17377] NA                "no inflammation" NA                "no inflammation"
#> [17381] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17385] "no inflammation" NA                "no inflammation" "no inflammation"
#> [17389] NA                NA                "no inflammation" "inflammation"   
#> [17393] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17397] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17401] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17405] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17409] NA                NA                NA                "no inflammation"
#> [17413] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17417] NA                "no inflammation" "no inflammation" NA               
#> [17421] NA                NA                "no inflammation" NA               
#> [17425] NA                NA                NA                NA               
#> [17429] "no inflammation" NA                "no inflammation" "inflammation"   
#> [17433] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [17437] NA                NA                "no inflammation" "no inflammation"
#> [17441] "inflammation"    NA                NA                "no inflammation"
#> [17445] "no inflammation" "inflammation"    NA                "inflammation"   
#> [17449] NA                "no inflammation" "no inflammation" NA               
#> [17453] "no inflammation" "no inflammation" NA                NA               
#> [17457] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17461] NA                "no inflammation" "inflammation"    "inflammation"   
#> [17465] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [17469] "no inflammation" NA                NA                NA               
#> [17473] NA                "inflammation"    "no inflammation" "no inflammation"
#> [17477] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17481] NA                NA                "inflammation"    "no inflammation"
#> [17485] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17489] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17493] NA                NA                NA                "inflammation"   
#> [17497] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17501] "inflammation"    "no inflammation" NA                NA               
#> [17505] "no inflammation" NA                "inflammation"    NA               
#> [17509] "no inflammation" "no inflammation" NA                NA               
#> [17513] NA                "inflammation"    NA                "no inflammation"
#> [17517] "inflammation"    NA                NA                "inflammation"   
#> [17521] "no inflammation" NA                NA                "inflammation"   
#> [17525] NA                NA                NA                NA               
#> [17529] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17533] NA                "inflammation"    NA                NA               
#> [17537] NA                NA                "no inflammation" "inflammation"   
#> [17541] NA                "no inflammation" "inflammation"    "no inflammation"
#> [17545] NA                "no inflammation" NA                NA               
#> [17549] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17553] "no inflammation" "no inflammation" NA                "no inflammation"
#> [17557] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17561] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17565] NA                "no inflammation" NA                "no inflammation"
#> [17569] "no inflammation" NA                "no inflammation" "inflammation"   
#> [17573] "inflammation"    NA                "no inflammation" NA               
#> [17577] NA                "no inflammation" NA                "no inflammation"
#> [17581] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [17585] "inflammation"    "no inflammation" "no inflammation" NA               
#> [17589] NA                "no inflammation" "no inflammation" "no inflammation"
#> [17593] "no inflammation" NA                NA                "no inflammation"
#> [17597] NA                NA                "no inflammation" "inflammation"   
#> [17601] NA                NA                NA                "no inflammation"
#> [17605] NA                "no inflammation" NA                NA               
#> [17609] "no inflammation" NA                "no inflammation" NA               
#> [17613] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17617] "inflammation"    "no inflammation" "no inflammation" NA               
#> [17621] NA                "no inflammation" NA                "no inflammation"
#> [17625] "inflammation"    NA                NA                NA               
#> [17629] NA                "no inflammation" NA                NA               
#> [17633] "no inflammation" "no inflammation" NA                "inflammation"   
#> [17637] "inflammation"    "no inflammation" NA                NA               
#> [17641] "inflammation"    NA                "inflammation"    NA               
#> [17645] NA                NA                "no inflammation" "no inflammation"
#> [17649] "inflammation"    "no inflammation" NA                NA               
#> [17653] "no inflammation" NA                "inflammation"    NA               
#> [17657] "inflammation"    NA                NA                "no inflammation"
#> [17661] NA                "no inflammation" NA                "no inflammation"
#> [17665] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17669] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [17673] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17677] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [17681] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17685] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [17689] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17693] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17697] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [17701] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [17705] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [17709] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17713] "inflammation"    "no inflammation" "no inflammation" NA               
#> [17717] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [17721] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17725] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [17729] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17733] NA                "no inflammation" "inflammation"    "inflammation"   
#> [17737] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17741] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17745] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17749] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17753] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [17757] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17761] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17765] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17769] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17773] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17777] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17781] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17785] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17789] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17793] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17797] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [17801] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [17805] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [17809] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [17813] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17817] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [17821] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17825] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17829] NA                "inflammation"    "no inflammation" "no inflammation"
#> [17833] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17837] NA                "no inflammation" "no inflammation" "inflammation"   
#> [17841] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [17845] "no inflammation" "no inflammation" "inflammation"    NA               
#> [17849] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17853] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17857] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [17861] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [17865] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [17869] "no inflammation" "inflammation"    "no inflammation" NA               
#> [17873] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17877] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17881] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17885] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [17889] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [17893] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17897] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17901] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [17905] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17909] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17913] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17917] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17921] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17925] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [17929] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17933] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17937] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17941] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [17945] NA                "inflammation"    "no inflammation" "inflammation"   
#> [17949] "no inflammation" NA                "no inflammation" "inflammation"   
#> [17953] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [17957] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [17961] "no inflammation" "no inflammation" "no inflammation" NA               
#> [17965] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17969] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [17973] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [17977] "inflammation"    "no inflammation" NA                NA               
#> [17981] "inflammation"    "no inflammation" "inflammation"    NA               
#> [17985] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [17989] "no inflammation" NA                "no inflammation" "inflammation"   
#> [17993] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [17997] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18001] "inflammation"    NA                "inflammation"    "inflammation"   
#> [18005] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18009] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [18013] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18017] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18021] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18025] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18029] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18033] NA                NA                "no inflammation" "no inflammation"
#> [18037] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18041] NA                "inflammation"    "inflammation"    "inflammation"   
#> [18045] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18049] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18053] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18057] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18061] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18065] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18069] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [18073] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18077] "inflammation"    NA                "inflammation"    "no inflammation"
#> [18081] "inflammation"    NA                "inflammation"    "no inflammation"
#> [18085] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18089] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18093] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18097] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18101] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18105] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18109] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18113] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18117] "inflammation"    "no inflammation" "inflammation"    NA               
#> [18121] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18125] NA                "inflammation"    "no inflammation" "no inflammation"
#> [18129] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18133] "no inflammation" NA                "inflammation"    "no inflammation"
#> [18137] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [18141] "inflammation"    NA                "no inflammation" "no inflammation"
#> [18145] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18149] NA                NA                NA                "inflammation"   
#> [18153] "no inflammation" "inflammation"    NA                NA               
#> [18157] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18161] "inflammation"    "inflammation"    NA                "no inflammation"
#> [18165] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18169] "inflammation"    "inflammation"    NA                "no inflammation"
#> [18173] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [18177] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18181] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18185] NA                "inflammation"    "inflammation"    "inflammation"   
#> [18189] "inflammation"    "inflammation"    "inflammation"    NA               
#> [18193] NA                "no inflammation" NA                "inflammation"   
#> [18197] NA                "inflammation"    "inflammation"    NA               
#> [18201] "inflammation"    "no inflammation" NA                "inflammation"   
#> [18205] "inflammation"    NA                "inflammation"    "inflammation"   
#> [18209] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18213] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18217] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18221] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18225] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18229] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18233] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18237] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18241] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [18245] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18249] "no inflammation" "no inflammation" NA                "no inflammation"
#> [18253] "no inflammation" NA                "inflammation"    "no inflammation"
#> [18257] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18261] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18265] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18269] "no inflammation" "inflammation"    "inflammation"    NA               
#> [18273] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18277] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18281] NA                "no inflammation" "no inflammation" NA               
#> [18285] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18289] NA                NA                "inflammation"    "inflammation"   
#> [18293] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18297] NA                NA                "no inflammation" "inflammation"   
#> [18301] NA                "no inflammation" "inflammation"    "inflammation"   
#> [18305] "inflammation"    "inflammation"    "inflammation"    NA               
#> [18309] "no inflammation" "no inflammation" "no inflammation" NA               
#> [18313] "inflammation"    "inflammation"    "inflammation"    NA               
#> [18317] NA                NA                "no inflammation" "inflammation"   
#> [18321] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18325] "inflammation"    NA                "no inflammation" "no inflammation"
#> [18329] "no inflammation" "inflammation"    NA                "no inflammation"
#> [18333] "no inflammation" "inflammation"    NA                "no inflammation"
#> [18337] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18341] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [18345] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18349] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18353] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18357] "no inflammation" "inflammation"    "inflammation"    NA               
#> [18361] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18365] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18369] NA                "inflammation"    "inflammation"    "no inflammation"
#> [18373] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18377] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18381] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18385] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18389] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18393] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18397] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18401] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18405] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18409] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18413] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18417] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18421] "no inflammation" "no inflammation" "no inflammation" NA               
#> [18425] "inflammation"    "inflammation"    "no inflammation" NA               
#> [18429] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18433] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18437] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18441] "inflammation"    "inflammation"    "no inflammation" "inflammation"   
#> [18445] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18449] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18453] "inflammation"    "no inflammation" NA                "inflammation"   
#> [18457] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18461] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18465] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18469] "inflammation"    "no inflammation" NA                "inflammation"   
#> [18473] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18477] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [18481] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18485] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [18489] NA                NA                "no inflammation" "inflammation"   
#> [18493] NA                NA                NA                "inflammation"   
#> [18497] NA                "inflammation"    NA                NA               
#> [18501] "inflammation"    "no inflammation" NA                NA               
#> [18505] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [18509] NA                "no inflammation" "inflammation"    NA               
#> [18513] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18517] NA                "no inflammation" "inflammation"    "no inflammation"
#> [18521] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18525] "no inflammation" NA                NA                "inflammation"   
#> [18529] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18533] NA                "no inflammation" "no inflammation" NA               
#> [18537] "inflammation"    "no inflammation" "no inflammation" NA               
#> [18541] "no inflammation" NA                "no inflammation" "no inflammation"
#> [18545] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [18549] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18553] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18557] "no inflammation" "no inflammation" NA                NA               
#> [18561] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18565] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18569] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18573] NA                "no inflammation" "no inflammation" NA               
#> [18577] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18581] "no inflammation" "no inflammation" "no inflammation" NA               
#> [18585] NA                "no inflammation" "no inflammation" "inflammation"   
#> [18589] "no inflammation" NA                "inflammation"    "no inflammation"
#> [18593] "inflammation"    "no inflammation" "no inflammation" NA               
#> [18597] NA                "no inflammation" "no inflammation" "inflammation"   
#> [18601] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18605] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18609] "inflammation"    "no inflammation" NA                "inflammation"   
#> [18613] "inflammation"    NA                "no inflammation" NA               
#> [18617] "no inflammation" "inflammation"    "inflammation"    NA               
#> [18621] "inflammation"    NA                "inflammation"    "no inflammation"
#> [18625] NA                "inflammation"    "no inflammation" "no inflammation"
#> [18629] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18633] NA                NA                "no inflammation" "inflammation"   
#> [18637] "no inflammation" "no inflammation" "inflammation"    NA               
#> [18641] "inflammation"    NA                NA                NA               
#> [18645] "no inflammation" NA                "inflammation"    NA               
#> [18649] "no inflammation" "inflammation"    NA                "no inflammation"
#> [18653] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18657] NA                "no inflammation" "inflammation"    "inflammation"   
#> [18661] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18665] "inflammation"    "no inflammation" "no inflammation" NA               
#> [18669] NA                NA                "inflammation"    "no inflammation"
#> [18673] "no inflammation" NA                "inflammation"    "no inflammation"
#> [18677] "no inflammation" "no inflammation" NA                "inflammation"   
#> [18681] "inflammation"    NA                NA                "no inflammation"
#> [18685] "no inflammation" NA                "no inflammation" "no inflammation"
#> [18689] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18693] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18697] "inflammation"    "no inflammation" NA                NA               
#> [18701] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18705] "no inflammation" "inflammation"    "inflammation"    "inflammation"   
#> [18709] "no inflammation" "inflammation"    NA                NA               
#> [18713] "no inflammation" "no inflammation" NA                "no inflammation"
#> [18717] NA                "inflammation"    "no inflammation" "inflammation"   
#> [18721] "no inflammation" NA                "inflammation"    "inflammation"   
#> [18725] "inflammation"    "no inflammation" "inflammation"    NA               
#> [18729] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18733] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18737] "no inflammation" NA                "no inflammation" NA               
#> [18741] NA                "no inflammation" "inflammation"    NA               
#> [18745] NA                "no inflammation" "inflammation"    NA               
#> [18749] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18753] "no inflammation" "inflammation"    "no inflammation" NA               
#> [18757] "inflammation"    "no inflammation" "no inflammation" NA               
#> [18761] NA                "no inflammation" NA                NA               
#> [18765] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18769] NA                "no inflammation" "inflammation"    NA               
#> [18773] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18777] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18781] NA                "inflammation"    "no inflammation" "no inflammation"
#> [18785] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [18789] "no inflammation" NA                "no inflammation" "no inflammation"
#> [18793] "inflammation"    "no inflammation" "inflammation"    NA               
#> [18797] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18801] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18805] NA                "no inflammation" "inflammation"    "no inflammation"
#> [18809] "no inflammation" "inflammation"    NA                NA               
#> [18813] "inflammation"    NA                "no inflammation" "no inflammation"
#> [18817] NA                NA                "no inflammation" "no inflammation"
#> [18821] "no inflammation" "inflammation"    NA                "inflammation"   
#> [18825] "no inflammation" "no inflammation" NA                "no inflammation"
#> [18829] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18833] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18837] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18841] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18845] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18849] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18853] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18857] "no inflammation" "no inflammation" NA                "no inflammation"
#> [18861] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [18865] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18869] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [18873] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [18877] "no inflammation" "no inflammation" "no inflammation" NA               
#> [18881] NA                "no inflammation" "no inflammation" NA               
#> [18885] "no inflammation" "no inflammation" "no inflammation" NA               
#> [18889] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18893] "inflammation"    NA                NA                "inflammation"   
#> [18897] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [18901] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [18905] "no inflammation" NA                "no inflammation" NA               
#> [18909] "no inflammation" "no inflammation" "inflammation"    NA               
#> [18913] "inflammation"    "no inflammation" "no inflammation" NA               
#> [18917] "inflammation"    NA                "no inflammation" "inflammation"   
#> [18921] "inflammation"    NA                NA                NA               
#> [18925] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [18929] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [18933] "inflammation"    "no inflammation" NA                NA               
#> [18937] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [18941] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18945] NA                "inflammation"    "no inflammation" "no inflammation"
#> [18949] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18953] "inflammation"    NA                "no inflammation" "no inflammation"
#> [18957] NA                NA                NA                "no inflammation"
#> [18961] "no inflammation" NA                "no inflammation" NA               
#> [18965] "inflammation"    "inflammation"    "inflammation"    "no inflammation"
#> [18969] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [18973] "no inflammation" NA                "inflammation"    "no inflammation"
#> [18977] "inflammation"    NA                "no inflammation" "no inflammation"
#> [18981] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18985] "inflammation"    NA                "inflammation"    "no inflammation"
#> [18989] NA                "no inflammation" "no inflammation" "no inflammation"
#> [18993] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [18997] "no inflammation" NA                "no inflammation" "inflammation"   
#> [19001] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19005] NA                "no inflammation" NA                "no inflammation"
#> [19009] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [19013] "no inflammation" NA                "inflammation"    "no inflammation"
#> [19017] "no inflammation" NA                NA                NA               
#> [19021] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [19025] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [19029] "inflammation"    "inflammation"    "no inflammation" NA               
#> [19033] NA                "no inflammation" "inflammation"    "no inflammation"
#> [19037] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19041] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19045] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19049] "no inflammation" NA                NA                NA               
#> [19053] NA                NA                "inflammation"    "no inflammation"
#> [19057] "no inflammation" "no inflammation" "inflammation"    NA               
#> [19061] NA                "no inflammation" "no inflammation" NA               
#> [19065] NA                NA                "inflammation"    "no inflammation"
#> [19069] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [19073] "inflammation"    NA                "inflammation"    "no inflammation"
#> [19077] "no inflammation" NA                "no inflammation" NA               
#> [19081] "no inflammation" "no inflammation" NA                NA               
#> [19085] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [19089] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [19093] "inflammation"    NA                "inflammation"    NA               
#> [19097] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19101] "no inflammation" "inflammation"    "inflammation"    "no inflammation"
#> [19105] NA                "inflammation"    NA                "no inflammation"
#> [19109] "inflammation"    "inflammation"    NA                "no inflammation"
#> [19113] NA                "inflammation"    NA                "no inflammation"
#> [19117] "inflammation"    "inflammation"    "inflammation"    "inflammation"   
#> [19121] "no inflammation" NA                "inflammation"    NA               
#> [19125] "no inflammation" NA                "no inflammation" NA               
#> [19129] "inflammation"    "no inflammation" "inflammation"    "inflammation"   
#> [19133] "inflammation"    "no inflammation" NA                NA               
#> [19137] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19141] NA                "inflammation"    "no inflammation" "no inflammation"
#> [19145] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19149] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19153] "inflammation"    "no inflammation" NA                NA               
#> [19157] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19161] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19165] NA                "no inflammation" "no inflammation" NA               
#> [19169] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [19173] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19177] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19181] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [19185] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [19189] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19193] "inflammation"    NA                NA                "no inflammation"
#> [19197] "no inflammation" "inflammation"    "no inflammation" NA               
#> [19201] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19205] "no inflammation" "inflammation"    "no inflammation" NA               
#> [19209] NA                NA                "no inflammation" "no inflammation"
#> [19213] "no inflammation" NA                "inflammation"    NA               
#> [19217] "no inflammation" "no inflammation" "inflammation"    "inflammation"   
#> [19221] "no inflammation" NA                "no inflammation" "inflammation"   
#> [19225] NA                "inflammation"    "no inflammation" "inflammation"   
#> [19229] NA                "inflammation"    "inflammation"    "inflammation"   
#> [19233] "no inflammation" NA                "inflammation"    "no inflammation"
#> [19237] "no inflammation" "no inflammation" "no inflammation" NA               
#> [19241] NA                "inflammation"    "inflammation"    "no inflammation"
#> [19245] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [19249] NA                NA                "no inflammation" "no inflammation"
#> [19253] NA                "inflammation"    NA                "no inflammation"
#> [19257] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [19261] NA                "no inflammation" "inflammation"    "no inflammation"
#> [19265] "no inflammation" NA                "no inflammation" "no inflammation"
#> [19269] "inflammation"    "inflammation"    NA                "no inflammation"
#> [19273] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [19277] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19281] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19285] NA                "no inflammation" "no inflammation" "inflammation"   
#> [19289] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19293] "inflammation"    "no inflammation" "no inflammation" NA               
#> [19297] "no inflammation" "no inflammation" NA                "no inflammation"
#> [19301] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [19305] "no inflammation" NA                "no inflammation" "inflammation"   
#> [19309] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19313] NA                "no inflammation" "no inflammation" "no inflammation"
#> [19317] "no inflammation" "inflammation"    "no inflammation" "inflammation"   
#> [19321] NA                "inflammation"    "no inflammation" "no inflammation"
#> [19325] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19329] NA                "no inflammation" "no inflammation" "inflammation"   
#> [19333] "no inflammation" "no inflammation" NA                "no inflammation"
#> [19337] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [19341] NA                "inflammation"    "no inflammation" "inflammation"   
#> [19345] "no inflammation" NA                "inflammation"    "no inflammation"
#> [19349] "inflammation"    "no inflammation" "inflammation"    NA               
#> [19353] NA                "inflammation"    "no inflammation" "inflammation"   
#> [19357] "inflammation"    NA                NA                "no inflammation"
#> [19361] "no inflammation" "no inflammation" "inflammation"    NA               
#> [19365] "inflammation"    "inflammation"    NA                "no inflammation"
#> [19369] "inflammation"    "no inflammation" "no inflammation" "no inflammation"
#> [19373] NA                NA                NA                NA               
#> [19377] "inflammation"    "no inflammation" "inflammation"    "no inflammation"
#> [19381] "no inflammation" NA                "inflammation"    "no inflammation"
#> [19385] "no inflammation" "no inflammation" "no inflammation" NA               
#> [19389] "no inflammation" "no inflammation" "no inflammation" "inflammation"   
#> [19393] "no inflammation" "no inflammation" "inflammation"    "no inflammation"
#> [19397] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19401] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [19405] "inflammation"    NA                "inflammation"    "inflammation"   
#> [19409] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19413] "inflammation"    "inflammation"    "no inflammation" "no inflammation"
#> [19417] "no inflammation" "inflammation"    NA                "inflammation"   
#> [19421] "no inflammation" "no inflammation" "no inflammation" "no inflammation"
#> [19425] "inflammation"    "no inflammation" NA                "inflammation"   
#> [19429] "inflammation"    "inflammation"    "inflammation"    NA               
#> [19433] "no inflammation" "inflammation"    "no inflammation" "no inflammation"
#> [19437] "inflammation"    "no inflammation" "no inflammation" "inflammation"   
#> [19441] "inflammation"    "no inflammation" "no inflammation" NA               
#> [19445] "no inflammation" NA                "no inflammation" NA               
#> [19449] NA               
detect_inflammation(crp = mnData$crp, label = FALSE)
#>     [1]  0  1 NA  0  0  0  0  0 NA NA  1 NA  0  1  1  1  1  0  0  0  1  1  1  0
#>    [25] NA  0 NA  0 NA NA  1  1  1  1  1  0  0  1  1  1  0  0  0  1  1  1  0  0
#>    [49]  0  1  1  1  1  0  0  0  0  0  1  1 NA  1  1  1  1  0  1  0  1  1  0  1
#>    [73]  1  1  1  1  1  1  1 NA  1  1 NA  0  1  0  0  0 NA NA  1  0  1  1  1  1
#>    [97] NA NA  1  0 NA  1  1  1  0 NA  1  1  1  1 NA  0  0  0  0  0 NA  0  1  0
#>   [121]  0  0  1  0  1  0  0  0 NA  1 NA  1  0  0  1  1  1  1  0  1  0  1  0  1
#>   [145]  1  1  0  0  1  1 NA  1  0  0  0  1  1  0  1  1 NA  0  1  1  0  1  1 NA
#>   [169]  0  1  1  0  1  1 NA NA  1  1 NA  0  0  1  0 NA  1  1  1  1  1  1  1  0
#>   [193]  0 NA  0  0 NA NA  1 NA  0  1 NA NA NA NA NA NA  1 NA NA  0  0  0  1 NA
#>   [217] NA  0 NA NA NA NA NA  0 NA NA NA  1  1  1 NA  1 NA NA  1  1  0  1  1  0
#>   [241]  0  0 NA  1 NA  0  1 NA  0  1  1  0  0  1  1  1  1  1  0 NA  1 NA  1  0
#>   [265] NA  1  0  1 NA  1  1  1  1 NA  1  1  1 NA  1  1 NA  1  0  0  1  0  1  1
#>   [289]  1  0  0  0  1  0  0  1 NA  0  1  0  0  0  0  0  1  0 NA  0  1  1  1  0
#>   [313]  1  0  0  0  0  1  0  1  0  1 NA NA  1  0 NA  0  0  1  0 NA  1  1 NA  0
#>   [337]  1 NA NA  1  1 NA  0  1 NA  1 NA  1  1  1  1  1  0  1 NA NA  0  0  0  0
#>   [361]  1  0  0  1  1  0  0  0  1 NA  1  1  0  1  0 NA  1  1  1  0  0  1  0  1
#>   [385]  0 NA  0  0  1 NA  1  1 NA  1  1  1  0  0  0  1  1  0  1  1  0  0  1  1
#>   [409] NA  1  1  1  0  0 NA  1 NA  1 NA  0  1  1  1  1  0  1  0 NA  0  0  1  1
#>   [433]  1  0  1  1  1  1 NA  0  1  1  0  0  0  0  1  1 NA  0  0  1  1  0  0  1
#>   [457]  0  1  1  0  1  0  0  0  0  0  1 NA  0  0  1  0  1  0  0 NA  1  1  1  1
#>   [481]  1  0  1  0  0 NA  0  0  1  1 NA  1  1  0 NA  0  1  0  1  1  0  1  1  0
#>   [505]  1  0  0  0  0  1  0  0  1 NA  0  0  1  1  0  1  1  1  1  0  0  0 NA  1
#>   [529] NA NA  0  0  1  0  0 NA  1  1  1  1  1  1  1 NA  1  0  0 NA  1  0  0  0
#>   [553]  0 NA  1  0  0 NA  1  1  0 NA  0  0  0  0  1  1  1  1  1  1  1  1  0  1
#>   [577]  1  1 NA NA  0  1  1  1  0  1 NA NA  1  1  0  1 NA  1  0 NA  1 NA NA NA
#>   [601]  1 NA NA NA NA NA NA NA NA  1  0  0 NA  0  1  1  0  0  1  0  1 NA NA  1
#>   [625] NA  0  0  1  0  1 NA NA  1  0 NA  0  1  1 NA  1  0  0  1  0  1  0  1  0
#>   [649] NA  0  1  0  0 NA NA  0  1  1  1  1 NA  0  1 NA  0  1  0  1  0  0  0  1
#>   [673]  0  0  1 NA  0  0  0  1  0  0  1  1  1  1 NA  0  0 NA NA NA  1  0 NA  1
#>   [697]  0  0  0  1  1 NA  0  1  1  1  0  0  1 NA NA NA  1  0  0  1  1  0  0  1
#>   [721]  0 NA  0  0 NA  0  0  1  1  1  1  1  0 NA  1  0  1  0  1  0  1  0  0  1
#>   [745]  0  1  0  1  1  0  1  0  0 NA  1  1  0  1  0  1  1  0 NA NA  1  0  1 NA
#>   [769]  0  0  1 NA NA  1  1  1  1  0  1  0  0  0  0  1  1  0  0  0  0  1  0  1
#>   [793]  1  0  0  0  1  0  0  0  1  0  1  0  0  0 NA  0  0  1  0  1  0  0  1  0
#>   [817]  0  0  0  1  0  0  1  0  0  0  0  1  0  1  0  0  1  1  0  0  0  1  0  0
#>   [841]  0  1  0  0  0  1  1  1  0  0  1  0  1  0  0  0  1  0  1  0  0  1  0  0
#>   [865]  0 NA  0  1  1  0  0 NA  0  0  1  0  0  1  1  0  0  0  1  0  1 NA  0  0
#>   [889]  0  0  0  1  1  1  1  1  0  1  1  1  1  0  1  0  1  1  0  1  1  0  0  1
#>   [913]  1  0  0  1  0  1  1  1  1  0  0  0  1  0  1  0  0  0  1  0  0  1  1  0
#>   [937]  1  1  0  1  0  0  0  1  1  0  0  1  0  1  1  1  0  1  1  1  0  0  0  1
#>   [961]  0  1  1  1  0  0  1  0  1  0  0  1  0  1  1  0  1  0  0  1  0  1  0  0
#>   [985]  1  0  0  0  0  0  1  0  1  1  0  0  0  0  0  1  0 NA  0  0  1  0  0  0
#>  [1009]  0  0  0  0 NA  1  0  1  0  0  0  0  0  1  1  1  1  0  0  1  1  0  1  0
#>  [1033]  1  1  0  1  0  1  1  1  1  0  1  0  0  1  1  0  0  1  1  0  1  1  0  0
#>  [1057]  0  0  1  1  1  1  0  1  1  0  1  0  1  1  1  1  1  0  1  1 NA  1  1  0
#>  [1081]  1  1  1  0  0 NA  0  1  0  0  1  0  1  1  0  1  1  1  0  0  0  0  1  0
#>  [1105]  1  1  1  0  0  0  1  0  1  1  1  1  0  1  0  0  1  1  0  0  1  1  1  0
#>  [1129]  1  1  1  0  0  0  0  0  0  1  1  1  1  1  1  1  0  0  0  0 NA  0  0  1
#>  [1153]  0  1  1  1  1  1  0  1  0  0  0  0  1  1  1  0 NA  0  0  1  1  1  1  0
#>  [1177]  0  0  0  1  0  0  0  1  0  1  0  0  0  0  0 NA  1  0  0  0  1  0  1  1
#>  [1201]  0  1  1  1  0  0  0  1  0  1  0  0  1  0  0  0  1  1  0  0  0  0  1  0
#>  [1225]  0  0  1  1  1  0  1  1  1  0  0  1  0 NA  0  1  0  0  1  0  1  0  0  0
#>  [1249]  1  0  0  0  0  1  0  1  0  0  1  1  0  0  1  1  0  1  1  0  0 NA  1  0
#>  [1273]  0  0  0  1  1  1  0  1  0  1  0  0  0  1  0  1  1  1  0  1  0  1  0  0
#>  [1297]  0  0  1  0  1  1  1  1  0  1  0  1  1  1  0  0  0  1  1  0  1  0  1  1
#>  [1321]  0  0  1  0  0 NA  0  0  0  1  0  0  1  0  0  0  0  0  0  0  1  0  1  0
#>  [1345]  0  0  1  0  1  0  0  0  0  1  0  0  1 NA  1  0  1  0  0  0  0  0  0  0
#>  [1369] NA NA  0  1  0  1  0  0  1  1  1  1  0 NA  1  0  0  0  0 NA  1  1  1  0
#>  [1393]  0  0  0  0 NA  1  1  1  1  1  1  0  1  1  0  0  0  1  0  0  0  1  1  1
#>  [1417]  0  0  0  1  1  0  1  0  0  0  0  0  1  1  0  1  0  1  1  0  0  0  0  1
#>  [1441]  0  1  0  0  1  1  0  0  0  0  0  0  0  0  0  1  1  0  0  1  0  1  0  1
#>  [1465]  1 NA  1  0  1  0  0  0  0  0  1  1  1  0  1  0  0  0  0  0  1  0  0  0
#>  [1489]  1  1  0  0  1  0  0  0  0  0  1  1  0  1  0  1  1 NA  0  1  0  0  0  0
#>  [1513]  0  0  1  1  0  1  0  1  0  1  0  1  1  0  1  0  1  1 NA  1  0  1  1  0
#>  [1537]  1  0  0  1 NA  1  1 NA  0  0  0  1  0  1  0  1  0  0  0 NA  1  1  1  1
#>  [1561]  0  1  0  1  1  0 NA  0  0  0  0  1  0  0  0  0  1 NA  1  0  0  1 NA  0
#>  [1585]  0  0  0  1  0  1  0  0  0  1  0  0  0  0  0  1  1  0  1  1  0  1  1  0
#>  [1609] NA  1  0  1  1  0  0  1  0  0  1  1  0  0  1  1  1  0  1  1  0  0  0  1
#>  [1633]  0  1  0  0  1  1  0  0  0  1  0  1  1 NA  0  0  0  0  0  0  0  0  1  1
#>  [1657]  1  1  0  1  0  0  0  0  1  0  0  0  0 NA  0  0  0  0  1  1  1  1  1  1
#>  [1681]  1  1  1  1  0  0  0  1  1  1  0  0  0  0  0  0  0  0  0  1  0  0  1  0
#>  [1705]  0  0  0  1  0  0  1  1  1  0  0  0  0  0  0  0  1  0  0  0  0 NA  0  0
#>  [1729]  0  1  1  1  1  0  0  0  0  1  1  1  0  1  1  1  1  0  1  1  1  1  1  1
#>  [1753]  0  0  0  0  0  1  1  0 NA  0  0  0  0  1  0  0  0  0  0  0  1  0  0  0
#>  [1777]  0  1 NA  0  0  0  0  1  1  1  1  1  1  0  0  1  0  0  1  0  0  0  1  1
#>  [1801]  1  1  0  0  1  0  0  1  1  1  0  1  0  0  0  0  1 NA  1  0 NA  1  1  0
#>  [1825]  1  0  0  1  1  1  0  0  0  1  0  1  1  0  1  0  1  1  0  1  1  0  0  1
#>  [1849]  1  0  0  0  1  0  0  0  0  0  1  0 NA  1  1  0  1  0  1  0  0 NA  0  0
#>  [1873]  1  1  0  0  0  1  0  1  1 NA  0 NA  0  1  0  0  0  0  1  0  0  1  0  0
#>  [1897]  0  0  0  1  0  0  0  0 NA  1  1  0  1  1  0 NA  0  1  1  0 NA  0  1  1
#>  [1921]  0  1  1  1  0  1  0  0  1  1  1  1 NA NA  0  0  1  0  1  1  1  1  1  0
#>  [1945] NA  0 NA  0  1  1  1 NA  1  1  0  0 NA  0  0  1  0  0  0  0  0  0  1  1
#>  [1969] NA NA  0  1  0  0 NA NA  1  1  1  1  0  1 NA  0  0  0  1  1  1  0 NA  0
#>  [1993]  0  0  1 NA NA  1  0  0  1  0  0  1  1 NA  0  0  0 NA  0  0  0  0  0  0
#>  [2017]  0  0  0  0  0  0  0  0  0  0  0  1  1 NA  0  0  0  1  0  0 NA NA  0  1
#>  [2041]  1  0 NA  1  1  1  0  0  0  1  0  0  0  1  1  0  0  0  1  0  0  0  0 NA
#>  [2065]  1  0  1  0 NA  0  1  0  0  0  1  0  0  1  0  1  0  1  1  1  0  1  1  1
#>  [2089]  0 NA  1  0  1  1  1  1  1  1  1  0  0  0  1  1  0  0  1  0  0  0  0  1
#>  [2113]  1  1  0  0  1  1  0  0  1  0  1  1  0  0  1  1  0  1  1  0  0  0  0  0
#>  [2137]  1  1  1  1  0  0  0  0  0  1  0  0  0  0 NA  0  1  0 NA  0  0  1  1  0
#>  [2161]  0  0  1  0  1 NA  1  0  0 NA  0  1  0 NA  0  1  0  1  1  0  0  0  0  1
#>  [2185]  0  1  1  0  1 NA  0  0  1  0  1  1  1  0  1  0 NA  1  0  1  0 NA  1  1
#>  [2209]  1  1  1  1  0  0 NA  1  0 NA NA  0  0  1  0  0  0  0  1  0  0  1  0 NA
#>  [2233]  1  0  0  1  1  0  0  1  0  1  0  1  0  0  1  0  0  0  1  1  0  1  0  1
#>  [2257]  0 NA NA  0 NA NA  0  1  1  0  0  0  1  0  0  1  1  1  0  0 NA  1 NA  1
#>  [2281]  1  1  0  0  1 NA  1  0  1  1  1  0  1 NA  0  0  1  0  1  0  0  1  0 NA
#>  [2305]  1  0  0  0  1  0  0  0  0  0 NA  0  1 NA  1 NA  0 NA  0  0  0  1 NA  1
#>  [2329] NA  1  0  0  0 NA  1  0  1  1 NA  1  0  0  1  0  0 NA  1  1  0  0  0  1
#>  [2353]  0  0  1  1  0 NA  0  1  1  0  0  0  0  1 NA  0  1  1  0  1  0  0 NA  1
#>  [2377]  1  0  0  0  1  0 NA  0  0 NA  0  0 NA  0  0  0 NA  0  1  1  1 NA  1  1
#>  [2401]  0  0  1 NA  0  1  1 NA  0  0  1  0  0  0  0  1  0  1  1  0 NA NA  0  0
#>  [2425]  0  0 NA NA  0  0  1  1  0  0  1  0  1  0  0  0  0  0  1  1  0  1  0  1
#>  [2449]  0  0  1  1  0  0  1  0  1 NA  1  1  0  1  1  1  0  1  1  0  1  1 NA  0
#>  [2473]  0  0  0  1  1  0  1  0  1  0 NA  0  0 NA  1  1  0 NA  0  0  0  0 NA NA
#>  [2497]  1  0  0  0  1  1 NA  0  0  1  1  1 NA NA NA  0  0  1  0  0  1  1 NA  1
#>  [2521]  1  1  0  0  0  0  1  0  1  0  1 NA  1  1  0  1  1  0  1  1  0  1  1  1
#>  [2545]  0  1  1 NA  0  1  0  1  0  0  0  1  1  0  1  0  0 NA  1  0  1  1  0  0
#>  [2569]  1  1  0  1  1  0  1  0  0  0  1  1  1  0  1  0  1  1  0  0  1  0  1  1
#>  [2593]  1  1  1 NA NA  1  1  1  0  0  1  0  0  1 NA  0  0  1  1  0  0  1  0  1
#>  [2617]  1  0  0  0  0 NA NA  0  0  1 NA NA  0  1  1  1 NA  0  1  0  0  1  0  0
#>  [2641]  1 NA  1  0  0  0  0  1  0  0  1  1  0  1  0  0  1  0  0  1  0  1  0  0
#>  [2665]  1  0  0  0  0  0  1  1 NA  1  0  0  0  0  1  1  0 NA  1  0  1  1  0  0
#>  [2689]  0  0  0 NA  1  1  0  0  1  1  1  1  0  1  0  0  0  0  0  1  1  1  1  0
#>  [2713]  0  0  0  1  0  1  0  0  1  1  0  1  0  0  0  1  0  1  0  0  1  1  0  0
#>  [2737]  0  1  1  1  1  0  1  0  0  1  1  1  1  0  1  1  0  0  1  0  0 NA  0  1
#>  [2761]  0 NA  1  0  0  0  0  0  1  1  1  0  0  1 NA  1  0  0  1  1  0  1  1  0
#>  [2785]  0  1  0  1  1  0  0  1  0  1  1  1  1  0  1  0  0  0  0  0  0  1  0  0
#>  [2809]  0  0  0 NA  0  1  0  0  1  0  0  1  1  0  1  0  1  1  1  0  0  1  0  1
#>  [2833]  1  0  1  1  0  1  1  1  1  0  0  0 NA  0  1  1 NA  1  1  0  1  0  1  1
#>  [2857]  0  1  0  0  0  0  0  0  0 NA  1  0  1  0  0  0 NA  1  0  1  1  0  0  0
#>  [2881]  0  0  1  1  1 NA  1  0  0  0  1  0  0  0  1  0  0  1  0  1 NA  0  0  0
#>  [2905]  1  0  1 NA  0  0  1  1  1 NA NA  1  0  0 NA  0  0  0  0  1  0  0  0  0
#>  [2929]  1 NA  1  1 NA  0  1  1  1  1  1  1  1  0  0  1  0  1 NA  0  0  0 NA  1
#>  [2953]  1  1  1  1  0  1  0  1 NA  0 NA  0  1  1  1  1  0  1  1  1 NA  1  1  0
#>  [2977]  0  1  1  0  1  0  0  1  0  1  0  1  0  0  1  1  0  0  0  0  1  0  1  1
#>  [3001]  0  1  0  0  0  0 NA  0  1  1  1  0  0  0  0  0  0  0  1  1  0  0  0  1
#>  [3025]  1 NA  0  1  0  1  0  0  1  0  0  0 NA NA  1  1  0  0  0  0  0  0  0  0
#>  [3049]  0  0  0  0 NA  0  0  0  0  0  1  0  0  0 NA  1 NA  0  1  0  0  1  1  0
#>  [3073]  0  0  0  0  1  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0 NA  0  1  0
#>  [3097]  1  0  0  0  0  1  0  1  1  0  0  0  1  1  1  1  1  0  0  1  0  1  1  0
#>  [3121]  1  0  1  1  0  0  0  0  0  1  1  0  0  0  0  0  0  1  0  1  1  0  1  0
#>  [3145]  0  0  0  0  1  0  0  1  1  0  1  1  0  0 NA  0  0  1  0  1  0  1  1 NA
#>  [3169]  0  1  0 NA NA  1  1  1  1  0  1  0  0  0  0  0  0  1  1  1  1  0  0  1
#>  [3193]  0  0  1  0  0  0  0  1  0 NA NA  1  0  1 NA NA  1  0  0  0  1  0  1  0
#>  [3217]  1  1  0  1  1  1  1  1  1  0  1  0  1  1  0  1  1  0  1  0  1  1  0  0
#>  [3241]  0  0  1  0  1  1  1  0  1  1  1  0 NA  1  0  0  1  0  1  0  0  0  1  0
#>  [3265]  0  1  1  0  1  0  1  0  0  1  1  0  1  1  0  0  1  0  0  0  0  0  0  1
#>  [3289]  1  0  0  1  1  0  1  0  1  0  0  0  0  1  0  1  0  0  1  0  0  1  0  0
#>  [3313]  0  0  0  1  0  1  0  1  0 NA  1  0  0  1  0  0  1  1  1 NA  0  1  1  1
#>  [3337]  0  0  1  1  0  0 NA  1  0  0  1  0  0  1  1  0  1  0  1  0 NA  0 NA  0
#>  [3361]  0  1  1  0  0  1  0  1  0  0  0  0  0  0  0  0  1  0  1  0  1  0  1  1
#>  [3385]  0  1  1  1  0  1  0  0  1  0  1  0  0  1  1  1  0  0  0 NA  0  0  0  1
#>  [3409]  1  1  0 NA  1  0 NA  0  0 NA  0  1  0  1  0  1  1  0  1  1  1  1  0  0
#>  [3433]  0  0  0  0  1  1  1  0  0  0  1  1  1  1 NA  1  1  1  0  0  1  0  1  1
#>  [3457]  0  0  0  1 NA  1  1  0  1  0  1  0  0  0  0  1  1  1  1  0  0  0  0  1
#>  [3481]  1  1 NA  0  1  0  1  0  0  0  1  1  0  0  0  0  0  1  0 NA  1  1  0  1
#>  [3505]  1  0  0  0  1 NA  1  0  0  0 NA  0  1  1  0  1  0  1  0  0  1  1  0  1
#>  [3529]  1  0  1  0  1  1  0 NA  0 NA  0  1  0  0  0  0 NA  0  1  0  1  1  1  0
#>  [3553]  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  1  0  1  0  0  0  1  1  1
#>  [3577]  1  0  0  1  0  1  0  1  0  1  1  0 NA  0  1  0 NA  0  0  0  0  0  1  1
#>  [3601]  0  0  0  0  1  0  0  0  0  1  1  0 NA  0  1  0  1  1  1  0  0  0  0  1
#>  [3625]  0  0  0  1  1  1  1  0  0  1  1  1  1  1  0  0  0  0  1  0  0  0  1  0
#>  [3649]  1  0  0 NA  0  0  0  1  1  1  0  1  0  1 NA  0  1  0  1  0  0  1  0  1
#>  [3673]  1  1  0  1 NA  1  1  1  1  0  0  0  1  1  0  0  0 NA  1  0  0  0  0 NA
#>  [3697]  0  0  1 NA  1  0  1  0  0  0  0  1  1 NA NA  0  1  0  1  1  1  1  1  0
#>  [3721]  0  0  0  0  0  1  0  0  0  1  1 NA NA  0  0  0  0  0 NA  0  0  0 NA  0
#>  [3745]  1  0  0  1  0  1 NA NA  0  0  0 NA  1  0  1  0  0  1  0  1  1 NA  1  1
#>  [3769]  1  1  1  0  0  1  0  0  0  1  1  0  1  0  1 NA  0  1  0  1  0  0  1  0
#>  [3793]  0  0  0  1  0  1  0  0  0  0  1  1  0  0  0  0  0  1  0  1  0  0  0  1
#>  [3817]  0  0  0  0  0 NA  0  1  1  0  1  0  0  1  0  1  1  1  0  0  0  1  1  1
#>  [3841]  1  1  1  0  0  1  0  1  1  1  0  0  1  0  0  1  1  0  1  1  1  0  1  1
#>  [3865]  1  1  0  0  0  0  0  1  0  0  1  0  1  0  0  0  0  0  1  1  1  1 NA  1
#>  [3889]  0  0  0  0  1  0  0  1  0  0  1  0  0  0  0  1  0  1  0  1  0  0  1  0
#>  [3913]  0  1  0  0  1 NA  1  0  0  1  0  1  1  0  1  1  1  0  0  0  0  0 NA  0
#>  [3937] NA  1  0  1  1  0  1  1  1  1  1  1  1  1  1  1  1  0  1  0  1  0  0  1
#>  [3961]  1  0  0  1  1  0 NA NA  0  1  0  0  1  0  0  1  0  1  0  0  1 NA  1  0
#>  [3985]  1  1  0  0  1 NA  0 NA  0 NA  0  0  1  0  0  0 NA  1  0  1  0  0  1  1
#>  [4009]  1  1  1  0  0  0  1  0  0 NA  1  1  0  0  0 NA  1 NA  0  0  0  1  0  0
#>  [4033]  0  1  1  0  0  1 NA  1  0 NA  1  0  0  0  1  1  0  0  1  1  1  0  1  1
#>  [4057]  1  1  0  1  0  0  1  0  0  0  1  1  1  0  1  1  1  0  1  1  0  1  0  1
#>  [4081] NA  1  0  0 NA  0  0  0 NA  1  0  0 NA  1 NA  0  1  1  0  0 NA  1  1  0
#>  [4105]  0  0  1  1 NA  1  0  1  0  0  0  0 NA  1  0  1 NA  1  1  0  1  1  1  0
#>  [4129]  1  0  1 NA  0  0  1  1  0  0 NA  1  1  1  1  0  0 NA  0  0  0  0  1  1
#>  [4153]  0  1  0  0  0 NA  1  0  1  0  0  0  1  0  0  0  0  1  0  1  0  0  0  0
#>  [4177]  1  0  0  0  0  1  1  0  1  0  1  0  1  1  0  0  0  1  0  0 NA  0  0  1
#>  [4201]  1  0  0  0 NA  0  1  1 NA NA  1  1 NA  1  0  0  0  0  1  1  0 NA  1 NA
#>  [4225]  1 NA  1 NA  0  0  0  1  1 NA  0  0 NA  0  0  0  1 NA NA NA  1 NA  1  0
#>  [4249]  0  1  0  1  1  0  1  1  1  0  0  1  1  1  0  0 NA  0 NA  1  1  0  1  1
#>  [4273] NA  1  1  0  0  0  1  1  0  1 NA  0  0  0 NA  1  0  0  1  1  0  0 NA  1
#>  [4297] NA NA NA NA  0  0  1  0 NA  0 NA NA  0  0  0  0 NA NA  1  0  1 NA  0 NA
#>  [4321]  0  1  1  0  0  1  0  0  0  1  0  1  0  0  0  0  0  0  0 NA NA  1  1  1
#>  [4345]  0  0  0  1  0  0  0  0  0  1  0  0  0  1  0 NA NA  1  0  1  0 NA  0  0
#>  [4369]  1  1  0  1  0  0  1  0  0  1  0  0  0  0  0  0  0  0  0  1  0  1  0  0
#>  [4393]  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  1  1 NA  1 NA  0  1  0  1
#>  [4417]  1  1  1  1 NA  0  1  1  0  1  0  0  1  1  0 NA NA  0  0  0  0 NA  1  0
#>  [4441]  0 NA  0  0 NA  1  0  1  0  1  0  0 NA NA  0 NA  0  1  1  1 NA NA  0  0
#>  [4465]  0  0  0  0  0  0  0  0  0  0  0  0 NA  0 NA  1  0  0 NA  1  0  1  0  0
#>  [4489] NA  1  0  0  0  0  0  1  0 NA  1  0  1  0  1  1  0 NA  0  0  1  1  0  1
#>  [4513]  1  0  0  1  0  0 NA  1  1  0  1  0  0  0  0  0  0  0  0  0  0  1 NA  0
#>  [4537]  0  0  0  0  0  0  0  0  1  0 NA NA  0 NA NA  1 NA  1  1  0  1 NA  1  1
#>  [4561]  1  1  1  0  0  1 NA  0  1  1  0  0  1  1  1  1  0  1  1  0  0  1  1  1
#>  [4585]  0  0  1  1  0  1  1  1  1  1  1  1  1  1  1 NA  1  0  0  0  1  1  0  1
#>  [4609]  1  1  1  1  1  1  1  1  1  1  0  0  0  0  0  0  0  0  1  0  0  1  1  0
#>  [4633]  1  0  0  1  1  0  0  0  0  0  0  1  0  0  0  0  0  0 NA  0  0  0  0  0
#>  [4657]  0  1  1  0  1  0 NA  1  0  0  0 NA  0  1  0  0  0  1  0  0  0  1  1  0
#>  [4681]  0  1  0  1  1  0  0  1  1  1  0  0  1  1  1  0  0  1  1  0 NA  0  0  0
#>  [4705]  0  0  1  1  0  0  0  1  0  0  0  1  0  1  0  0  0  1  0  0  1  1  0  0
#>  [4729]  0  0  1  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  0 NA  0 NA
#>  [4753]  0  0  0  1 NA  0  0  1  0  0  0  0  1  1 NA  0  0  1  0  1 NA  0  0  1
#>  [4777]  1 NA  1  0  1  0  0  0  0  0  0 NA NA  0  0  0  0  0  0  0  1  1 NA  0
#>  [4801]  0  1 NA NA  0  0  0  0  0  1 NA  0 NA NA  0 NA  1  0  1  0  0  0 NA NA
#>  [4825]  0  0  0  0  0  0  1  0  0  0  1  0  0  1 NA  1  0  1  1  0 NA  0  0  0
#>  [4849]  0  0  0 NA  0  0 NA NA  1  0  1  0  0  0 NA NA  0  1  0  1 NA  0  0  1
#>  [4873]  0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1  1  0  1  1  0  0
#>  [4897]  0  0 NA  0  0  1  1  0  0 NA  0  0  1  0  1  0  0  1  1  0  0  0  1  0
#>  [4921]  0  0  1  1  0  0  0 NA  0  0  0  1  0  0  0  1 NA  1  0  1  0  1  0  0
#>  [4945]  1  0  0  0  0  1  0  0  0  0  1 NA  0  0  0  0  0  1  1  1  0 NA NA  1
#>  [4969]  0  0  0  0  0  0  0 NA  1  0  0  0  0  0  0  0  0  0  1  0  1  0  0  0
#>  [4993]  1 NA  0 NA  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1  1  0  0  1
#>  [5017]  0  0  0  1  1  0  0  0  0  1  0  1  0 NA  0  1  0  1  0  1  0  0  1 NA
#>  [5041]  0  0 NA  0  1  1 NA  0  0  0  0  1  0  1  0  1  0  1  0  0  0  1  0  0
#>  [5065]  0  0  0  0  0  0  0  0  0  0  1 NA  0  0  0  1  0  0  0  1  1  0  0 NA
#>  [5089]  0  0  1  1  0  0  0  0  0  0  1  0  0  0  1  0  1  0  0  0  0  0  0  0
#>  [5113]  0  1  1  0  0  0  0  0  0  1  0  1  0  0  0  1  0  1  1  1  0  1  0  1
#>  [5137] NA  1  1  0  0  0  0  1  1  1  0  1 NA  1  0  1  1  0  1  0  0  1  1  0
#>  [5161]  1 NA NA  0  0  0 NA  1 NA  0 NA  0  1  0  0 NA  1  0 NA  0  1  1 NA  0
#>  [5185]  0  1 NA NA  0 NA NA  0  1  1 NA NA  0  1  0  0 NA  0  1  1  1  0 NA NA
#>  [5209]  0  0  1  1  0  1  1  1 NA NA NA  1  0  1  0  0  1 NA  1  1  1  1  0  1
#>  [5233] NA  0  1  0  0  0  0 NA  0  0  1  1  1  0  1  1  0  1 NA NA  1  1 NA  0
#>  [5257] NA  1  0 NA  0  1  1  1 NA  0  0  0 NA  0  0 NA  1  1  1 NA  0 NA  1  1
#>  [5281] NA  0 NA  0 NA NA  1  0  1  0 NA NA NA NA NA  1  0 NA  1  1  1 NA NA NA
#>  [5305] NA  0  1 NA  0 NA  0  0 NA NA  1 NA  0  0 NA  1  0  0  0  0  0  0 NA NA
#>  [5329]  1  0 NA NA NA NA NA  0  1 NA  0 NA NA NA NA NA  0 NA NA  1  1 NA  1 NA
#>  [5353] NA  0 NA  1 NA NA NA  0 NA NA  0 NA  1 NA  1  0  1 NA  1 NA  1  1  1 NA
#>  [5377]  1  1  1 NA NA NA  1  1  1  1 NA NA NA  0  0 NA NA NA  1  0 NA NA  1 NA
#>  [5401]  1  1 NA NA  0 NA  1  1  1  1  0  1  1  1  0  0 NA NA  0  1  0  0  0 NA
#>  [5425]  1  0 NA NA  0  0  1  0  1  0  1  0 NA  0  0 NA  0  1  1  0  1 NA  1 NA
#>  [5449]  1 NA  0  0  1  0  0  1  1 NA  0 NA NA  1  1  1  1 NA NA NA  0  1  1  1
#>  [5473]  0  1  1  1  1 NA  0  0  1 NA  1 NA  1  1 NA  0  0  1  0 NA  0 NA  0  0
#>  [5497]  1  0  1  0  1  0  1  0  0  0  0  0 NA  0  0  0  0 NA  1  0  0  0  0 NA
#>  [5521]  1 NA NA  1 NA  0 NA  0  0  0  0  0  0 NA NA  0 NA  0  0  1  1  0 NA  1
#>  [5545]  0  0  0  0  0  0  0  0 NA NA  0  0  1 NA  0  0 NA  1 NA  1  0  1 NA  1
#>  [5569]  0  0 NA  0 NA  1  1  1  0 NA NA  1  1  1  0  1  1  0  0  0 NA NA  0 NA
#>  [5593]  0  1 NA NA  1  1  1  1 NA NA  1  1  1  0 NA  0  1 NA  0  0  1 NA NA  0
#>  [5617] NA  1  1  0 NA  1  1  0  1  1  0 NA  1 NA  0  1 NA  1  1  1  1  0 NA  1
#>  [5641]  1  1  1 NA NA NA  1 NA  1  1 NA  1 NA  0  1  1  1  1  1  0 NA  1  1 NA
#>  [5665]  0 NA  0 NA  1  0 NA  1  1  1  0  1 NA  1 NA  0  0 NA  0  0  0  1  1  0
#>  [5689] NA NA  0  0 NA  1 NA NA  1  0  1  0 NA  1  0 NA  0 NA NA  1  1  0  0  1
#>  [5713] NA  0  0  1 NA  0  1 NA  1 NA NA  0  1  1  1  0  1  1 NA  1  1  1  0 NA
#>  [5737]  0  1  1  1  0  0  1 NA  1  1 NA  0  0  0 NA  1  0  0  0  0  1 NA NA  0
#>  [5761]  0  0  0  0  1  0  0  0  0  1 NA NA  0  1  0  0  0  1 NA  0  0  1  0 NA
#>  [5785] NA  0  0  0  1  1 NA NA  0 NA  1 NA NA  0 NA NA  0  1  0  0  0  1  0  0
#>  [5809] NA NA NA NA  0  0  0  0 NA  1  0 NA  0  0  1  0  0 NA NA NA NA NA NA NA
#>  [5833] NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA NA
#>  [5857] NA NA NA NA NA NA NA NA NA NA  0  0 NA  1  1  1  1  0  1  0  0  0  1 NA
#>  [5881]  1  1  0  1 NA  1  0  1  1  1  1 NA  1  0  1  1  0 NA  1  1  1 NA  1  1
#>  [5905]  0  1  1  1  0  1 NA NA NA  1 NA  0  0  0  0  0  1 NA NA  1  0 NA  0  0
#>  [5929]  0 NA  0  0 NA NA  1 NA NA NA NA  0  0 NA NA  0  1 NA  0 NA NA NA NA  1
#>  [5953]  0  0  1  1  0  1  1  1  0  1  0 NA  0  1  1  1  0 NA  0 NA  0 NA  0  1
#>  [5977]  1  1 NA  0  0  0  0  1  1  1  0  0  0  1  0  1 NA  0  0  1  0  1  0  1
#>  [6001]  0  0  0  1  0  0 NA  0  0  0 NA NA  1  0 NA  1  1  0 NA NA  1  0  1 NA
#>  [6025]  1  0 NA  1  0  0  1  0  0  1  1  0  0  1  1  1  1  1  1  1  1  1  1 NA
#>  [6049]  1  0  1  1  0  1  1  1  1 NA  0  1  1 NA  1  1  1 NA  1 NA  1  1  1  1
#>  [6073] NA  1  1  1  1  1  1  1  1 NA  0  1 NA  1  1  1  1 NA NA  1  1 NA NA  1
#>  [6097]  1  1  1  0 NA  1  1  0  1 NA NA  1 NA  1  0  1  1  1  1  1 NA NA NA  1
#>  [6121] NA  1  1  1 NA  1  1 NA  0  1  1  0  1  0  1  0 NA  1  0  1  1  0  1  0
#>  [6145]  0  1  1  1  1  1  1 NA  1 NA  0 NA  1  1 NA  1  1  0  1 NA NA  0  0  1
#>  [6169]  1 NA  1  1  1  1  1  1 NA NA  1  1 NA  1  1 NA  1  1  1 NA  1  0  1  1
#>  [6193]  1  0  0  1  1  0  1  1  0  1  1  1  0  1  0  1  0  0  0  0  1 NA NA NA
#>  [6217]  1  1  1  0  1  0  0  0  0  0  1  0  1  1  1  0  0 NA NA NA NA  0  0  0
#>  [6241]  0  0  0  1  0  0  1  0  0 NA  0  1  1  0  0  0  1  1  0  0  0  0  0  0
#>  [6265]  1 NA  0  0  0  0  0  0  1  1  0  0 NA  0  0  0  0  0  0  0  0 NA  0  0
#>  [6289]  1  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1  1  0 NA  0  0  1 NA  1
#>  [6313]  1  1  0  1  1  0  1 NA  0  0  0  0  0 NA  1  1  1  1  0  1  1  0  1  1
#>  [6337]  0  0  0  1  0  0  1  0  1  1  1  1 NA  0  1  0  1  1  0  1  0  1  0  0
#>  [6361]  1  1  0  1  1  0  0  1  0  0  0  0  1  0  0  1  0  0  1  1  0  1  1  0
#>  [6385]  1  1  0  0  0  0  0  1  0  1  0  0  1  1  1  0  1  0  0  0  1  0  0  0
#>  [6409] NA  1  1  1  0  0  1  1  1  1  1  1  1  1  1  1  0  1  0  1  1  0  0 NA
#>  [6433]  1  0 NA  0  0  0  1  0  0  1  0  0  0  1  0  0  0  0  0  1  0  1  0  0
#>  [6457]  0 NA  0  0  0  1  0  0  0  0  1 NA  0  1  1  0  1  0  0  1  1  0  0  1
#>  [6481]  0  1  1 NA  0  1  0  0  0  0  0  0  1  0  1  0  0  1  1  0  0  0  0  0
#>  [6505]  0  0 NA NA  0  0  0  0  0  1  0  0  1  0  0  0  0  0  0  0  1  1  0  0
#>  [6529] NA  0  0  0  0  0  1  1  0  0  1  0  0  1  0  1  0  0  0  1  0  0  0  1
#>  [6553]  0 NA NA  0  0  0 NA NA  0  0  0  1  1  0  1  0  1  0  1  0  0  1  0  1
#>  [6577]  0  1  0  1  1  0  0  0  0  1 NA  0  1  0  0  0 NA  1  0 NA  0  0  0  0
#>  [6601]  1  0  0  0  0  0  0  1  1  0  0  0  0 NA  0 NA NA  0  1  0  0  1  1  0
#>  [6625]  1  1  0  0  1  1  1  1  1  0  0  0 NA  0  0  1  0  0  1  0  0  0  1  1
#>  [6649]  1  0  0  1  1  0  1  1  1  1  1  1  0  0  1  0  1  0 NA  0  0  0  0  1
#>  [6673]  1 NA  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0 NA  0  1  0
#>  [6697]  0  0  0  0  0  0  1  0  1  1  0  1  0  1  0  1  1  0  0  1  1  0  0 NA
#>  [6721]  1 NA  1  0  0  0  0  0  0  0  0  1  1  0 NA  1  0  1  1  0  0  0  0  0
#>  [6745] NA  1  0  0  1  0  0  0  1  0  0  0  0  0  0  0 NA  1  1  0  0  0  0  1
#>  [6769]  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  1  1
#>  [6793]  1  0  0  0  0  0  0  0  1  0  0  1  1  1  0  0  0  0  0  0  0  0  0  0
#>  [6817]  1  0  0  0  0 NA  1  0  1  0  1  0  0  0  0  1  0  0  0  1  0  1  0  0
#>  [6841] NA  1  0 NA  1  0  0  1  0  1  1  0  1  0  1  1  0  0  0  1  1  1  0  0
#>  [6865]  1  0  0  1  1  1  0  0  1  1  0  0  0  1  1  0  1  1  1  0  1  0  0  0
#>  [6889]  0  0  0  1  0  1  0  0  1  0  0  1  1 NA  0  1  1  0  0  1  0  1  0  0
#>  [6913]  1  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  1  0  0  0  0  1  0  0
#>  [6937]  1  0  1  1  0  1 NA  0  1 NA  1  0  1  0  0  0  0 NA NA  0  1  0  0 NA
#>  [6961]  0 NA  0  0  1  0  0  1  0  1  1  1  0  0  0  0  0  1  1  0  0  0  0  0
#>  [6985]  0  1  0  0  0  0  0  0  0  0  0  1  0  0  0  0  1  0  0  0  0  0  0  1
#>  [7009]  1  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  1  0  0  0  0  0  0  0
#>  [7033]  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  1 NA  1  1  0  0
#>  [7057]  0  0  1  0  0  0  0  0  0  0  0  1  1  0  0  1  0  0 NA  0  0  0  1  0
#>  [7081]  0  0  1  0  1  0  0  0  0  1  0  0  0  0  0  0  1  0  0  0  0  0  1  0
#>  [7105]  0  0  0  1  0  1  0  0  1  1  0  1  1  0  0  0  0  0  1  0  0  0  0  1
#>  [7129]  0  1 NA  0  0  0  0  1  1  1  0  0  0  0  0  0  0  0  0  0  1  1  1  0
#>  [7153] NA  0  1  1  0  0  0  1  1  1  0  0  0  1  0  1  0  0  0  0  0  0  1  0
#>  [7177]  0  0  1  0  0 NA  1 NA  0  1  0  0 NA  0  0  0  0  0 NA  1  1  0 NA  0
#>  [7201] NA  0  1  0  0 NA  1  0  0  1  0  0  0  0  0  0  1  1  0  1  0  0  0  0
#>  [7225]  0  0  0  0  0  0 NA  0  1  0  0  0  0  0  0  0 NA  0  1  0  0  0  0  0
#>  [7249]  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  1  1
#>  [7273]  0  0  0  0  0  1  0  1  1  0  1  0  1  0  0  1  0  0  0  0  0 NA  0  0
#>  [7297]  0  0  1  1  0  0  1  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0
#>  [7321]  0  0  0  0  0  0  0  0  1  0  0  1  0  0  1  1  0  1  0  0  0  0  0  1
#>  [7345]  0  0  0  0  0  0  0  0  0  1  1  0  0  0  0  0  1  0  1  0  0  0  1  0
#>  [7369]  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1
#>  [7393]  0  0  1  0  0  0  1  1  0  0  0  0  0  0  1  0 NA  1  0  1  0  0  0  0
#>  [7417]  0  0  0  0  0  0  0  0  0  1  0  1  1  1  0  0  0  0  0  0  1  1  1  0
#>  [7441]  0  1  0  1  1  0  1  0  0  0  0  0  0  1  0  0  1  1  0  0  1  0  0  0
#>  [7465]  0  0  0  0  0  1  0  0  0  0  1  0  1  1  1  0  0  1  1  0  1  0  1  0
#>  [7489]  0  0  0  0  0  1  0  0  0  0  1  0  0  0  0  1  0  0  0  1  0  0  1  0
#>  [7513]  0  1  0  0  0  1  1  0  0  0  0  0  0  1  0  0  0  0  0  0  1  0  1  0
#>  [7537]  1  0  0  0  0  0  0  0  1  1  0  0  0  1  0  0  0  0  0  0  0  0  0  0
#>  [7561]  0  0  0 NA  0  0  0  1  0  0  0 NA  0  0  0  0  1  1  1  1  0  0 NA  1
#>  [7585]  0  1  0  0  1  0  0  1  0  0  0  0  0  0  1  1  1 NA  1  0  1  0  1  1
#>  [7609]  0  0  0 NA  1  0  0  0  1  0  0 NA NA  0 NA NA  1  1  0  0  0  0  0  0
#>  [7633]  0  0  0 NA  0 NA  0 NA NA  0 NA NA  0 NA NA NA NA NA  0  1  0  1  0  0
#>  [7657] NA NA NA NA  0 NA NA NA  1  1  1  0  0  0  0  1  0  0  0  0  0  0  1  0
#>  [7681]  0  0  0  0  1  0  1  0 NA  0  0  1  0  0  0 NA  0  0  0  0  0  1  0  0
#>  [7705]  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  1  1  0
#>  [7729]  0  0  1  0  0  0 NA  0  0  0  0  0  1  1  0  0  0  1  1  0  0  0  0  0
#>  [7753]  0  0  0 NA  0  0  1  0  0 NA NA  0  0  1 NA  0 NA  1  0 NA NA  0  0  0
#>  [7777]  0  0  1  0  0  0  0  1  0 NA  0 NA  1  0 NA NA  0  0  0  1 NA  0  0 NA
#>  [7801]  1  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0 NA  0  0  0  0  0  1 NA
#>  [7825] NA  0  0  0  0  0  0  0  1  0  0  0  1 NA  0  1 NA  0 NA  0 NA  0 NA  0
#>  [7849]  0  0  0 NA  0  1  0  0  1  0  1  0  0  0  1  0  1 NA  0  0  0  0  1  0
#>  [7873]  0  0  1  1  1  0 NA  0  0  1  1 NA  1 NA  0 NA  0  1  0  0  1  1  0  0
#>  [7897]  0  1  0  0  0  0  1  0  1  0  0  1  0  0 NA  0  0  0  1  1  0  0  0  1
#>  [7921]  0  1  0  1  0  0  0  1  1  0  1  0 NA NA  0  0 NA  0  1 NA  1  1  0  0
#>  [7945]  0 NA  0  1 NA  0  0 NA  0  0 NA NA  0  0  0  0  0  0  0 NA  1 NA NA NA
#>  [7969] NA NA  0 NA NA  0  0 NA  0 NA  0  0  0 NA NA  0  0  1  0  0  1  0  0  0
#>  [7993]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0
#>  [8017]  0  0  0  0  0  0  1  0  0  0 NA NA NA  0  0 NA  0  0  0  0 NA  0  0  0
#>  [8041] NA  0  0 NA  0  0 NA  0 NA  0  0  0  0  0 NA  0 NA NA  0  0  0  1  1  0
#>  [8065]  1  0  0  0  0 NA  0  0  1  0  0  0 NA NA  0  0  0  0  0  0  0  0  0  0
#>  [8089]  1  0  0  0  1  0  0  0  0  1  0  0  0  1  0  0  0  0  0  0  0  0  0  1
#>  [8113]  0  0  0 NA  0  0 NA NA NA  1  0  0 NA  1  0  0  0  0  0  0  0 NA  0  0
#>  [8137]  0  0  0 NA  0  0  1 NA  0 NA NA  0  0  0 NA  0 NA  0 NA  0 NA  0 NA  1
#>  [8161]  0  1  1  0  1  0 NA  0  0  0  0  0  0  0  0  0  0  1  0  1  0  0  0  0
#>  [8185]  0 NA NA  1  0  0  0  0  1  0  0  0  0  0  0  0  0  0 NA  1  0  0  0 NA
#>  [8209] NA  0  0  0 NA  1  1  0  0  0  0  0  0  0  0  1  0 NA  0  1 NA  0  0  0
#>  [8233]  0  0  1  0  0  1  0  0  0  0  1  0  0  0  1  1  0  0  0  1  0  0  1  0
#>  [8257]  0  0  0  1  0  0  0  1  0  0  1  1  1  1  0  1  0  0  0 NA  0  1  0 NA
#>  [8281]  0  1  0  0  0  0  1  1  0  0  1  0  0  0  0  0  1  1  0  0  0  0  0  0
#>  [8305]  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  1  0  1  0  0  0  0  0  0
#>  [8329]  0  0  0  0 NA NA  0  0  1  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0
#>  [8353]  1  0  0  0  1  0  0  0  1  0  1  1  0  0  0  0  1  0  0  0  0  0  1  0
#>  [8377]  0 NA  0  0 NA  0  0  1  1  1  1  1  0  1  0  0  0  0 NA  0 NA NA NA NA
#>  [8401]  0 NA  0 NA NA  1 NA NA NA  0  0 NA NA  0  0  0  0  1  0  0  0  0  0  0
#>  [8425]  0  0  0  0  0  0  0  1  0  0 NA  0  0  1  0  1  0  0  0  1  0  0  0  0
#>  [8449]  0  0  0  0  1  0  0  0  0  1  0  0  1 NA NA NA  0  0 NA NA NA  1 NA NA
#>  [8473]  0 NA NA  1 NA  0 NA  0 NA  0  0 NA NA NA  1  1  1  0  1  0 NA NA  1  0
#>  [8497]  1 NA NA  0  0  0  0  0 NA  1  0  0  0  0 NA NA NA NA  0 NA  0  1 NA  0
#>  [8521]  0  0  0  1  0  1  1 NA NA  0  0  0 NA  0  0  1 NA NA NA NA NA NA NA NA
#>  [8545] NA NA  0  1  0 NA  0  1 NA  1  0 NA NA NA  0 NA  0  0 NA NA  1  0  1  1
#>  [8569] NA NA  0  0  0  0  0 NA  0  0  1  0 NA  1  0 NA  0  0 NA NA NA  1  0  0
#>  [8593]  1  0  1  1  0  0 NA NA  0 NA  0 NA  0  0  0 NA  0  0  0  0  0  0  0 NA
#>  [8617]  1 NA  0  0  0  0  0  0  1 NA NA NA NA  0  0 NA  0 NA  0 NA  0 NA NA NA
#>  [8641] NA  0 NA  0  0  0  0 NA NA  1 NA NA  0  0  0  0  1  0 NA  0 NA  0 NA  0
#>  [8665] NA  0  1 NA  0 NA NA  0  0 NA  0  1  1  0  0  0  1  0  0  0  1  0 NA NA
#>  [8689]  0 NA  0  0  0 NA  1  0 NA NA  1 NA  1  0 NA NA NA NA  0  0  0  0 NA  1
#>  [8713]  1 NA NA  0  0  1  0  0 NA  0 NA  1 NA  1 NA NA  0  0 NA NA  1  0  0  0
#>  [8737] NA NA  0  0  0  0 NA  1  0 NA  0 NA  0  0  0  0 NA  1  0  0  0 NA  0  0
#>  [8761]  0  0 NA NA NA NA NA NA  1  0  0  0  0  0  0  1  0  0  0 NA  0  1 NA  0
#>  [8785]  0  0  0  0  0  0  0  0  0 NA  0 NA  0  0  0  0 NA  0  0  0  0  0  0  0
#>  [8809]  0  0  0  0  0  0  0 NA NA  1  0  0  0 NA  1  1  0 NA  1  1 NA  0  1  0
#>  [8833] NA NA  1  0  0 NA  0  0 NA  0  0  1  0  0  0  1 NA  1 NA  0  0  0  0  1
#>  [8857]  0 NA  0  1  0  1  0  0 NA  1  0 NA  0  0  0 NA NA NA NA  0  0  0  0 NA
#>  [8881]  1 NA  0  1 NA  1  0 NA  0 NA  0 NA NA  0  0 NA NA  1 NA NA NA  1  0 NA
#>  [8905]  1  0 NA  0  0 NA NA  0 NA NA NA  1  0  0 NA  0  0  0  0 NA  0 NA NA NA
#>  [8929]  0  0  0  1 NA  0 NA  0  0  0  0  0  0  1  0  0 NA NA  0 NA NA  0  1 NA
#>  [8953]  0  1  0  1  0  0  0 NA  0  0  0  0 NA NA  0  1 NA  1 NA NA  0  0 NA  0
#>  [8977]  0 NA  0  0  0  1  0  0 NA NA  1  0  0 NA  1 NA  0  1  0 NA NA NA  0 NA
#>  [9001]  0  0  0 NA  0  0 NA NA NA NA  0  1  0  0  0  0 NA  1  0  0  0  0  1  0
#>  [9025]  1 NA  0 NA  0  0  1 NA NA  0  0 NA  0 NA  0  1 NA  0  0 NA NA NA  1  0
#>  [9049]  0 NA  0  1  0  0 NA NA NA  0  0  0  0  0  1  0 NA NA  1  1 NA  1 NA NA
#>  [9073]  0  0 NA NA NA  1 NA  1  0  0  0  0 NA  0  0 NA  0 NA  0  0  0  0  0  0
#>  [9097]  0  0 NA  0  0 NA NA NA  0  0  0 NA  1  0 NA NA NA  0  0 NA  0  0  0  0
#>  [9121]  0  0  0  0  1 NA NA NA  0  0  0  1  1 NA NA  0 NA  1  0  1  0  0  1  0
#>  [9145] NA  0  0  0 NA  1  1  0  0  0  0  1  0  0  1  0  1 NA NA  0  0  0  0  0
#>  [9169]  0  0 NA  0  0  0  0  0 NA NA  1  0 NA  0  1  0  1 NA  0  1  0 NA NA NA
#>  [9193]  0  0  0  0 NA NA  0 NA  0  0 NA NA NA  0  1 NA  0  0  0 NA  0  0  0  0
#>  [9217]  0 NA  1 NA  0  0  0  0  0  0 NA NA  0 NA  0  1  0  0  0  0  0  0  1  0
#>  [9241] NA  0  1  1  0  0 NA  0  1  1  1  0 NA NA NA  1  0  0  0 NA  0  0  1  0
#>  [9265] NA NA NA  0  0  0 NA  0  0 NA  0  0 NA  0  0  0  0 NA NA  1  0 NA  0 NA
#>  [9289]  1  0  0  0  0  0 NA  0 NA  0 NA NA NA  0  0  0  0  0 NA  0 NA  0 NA NA
#>  [9313]  0  0 NA NA  0  1  0 NA  1  0 NA  1  0 NA NA  0  0  1  1  0 NA  0  0 NA
#>  [9337]  0  0  1 NA  0 NA  0 NA  1  1  0  0 NA  0 NA  0 NA  1  0  0 NA  0 NA NA
#>  [9361]  0 NA NA NA  0  0  0 NA NA NA  0 NA NA  0 NA NA NA  0  0 NA  0 NA NA NA
#>  [9385]  0 NA NA NA NA NA  1  0  0 NA  0  0  1  0  1  1  0  0  1  0 NA NA  0 NA
#>  [9409] NA  0 NA NA  0  0 NA  0  1  1 NA NA  0  0  0 NA  0 NA  0  0  1  0  1  0
#>  [9433] NA  1 NA  0  0  0 NA NA NA  0 NA  0  0  0 NA  0 NA NA NA NA NA  0 NA  1
#>  [9457]  0  1 NA  0  1  1  0 NA  0  0 NA  0  0  0  0  0  0 NA  0 NA NA  0  0  0
#>  [9481] NA  1 NA  1  0  0  0  0  0  0 NA  0  1 NA  1 NA  1  1  0  1  1  0 NA  0
#>  [9505] NA NA  1  1  1  1  0  0  1  0 NA  0 NA NA NA NA NA NA  0 NA  0 NA NA NA
#>  [9529] NA NA  0 NA  0 NA  1 NA  0 NA NA NA NA NA NA  0  0  0  0 NA  0 NA NA  0
#>  [9553]  0  0  0  0  1 NA  0 NA  0 NA  0 NA  0  0  0 NA  0 NA NA  0  0  0  0  0
#>  [9577]  0  1  0  0  0  0  0  0 NA  0 NA  0  0  1 NA  0  0  1  1 NA  0  0  0  0
#>  [9601]  0  1  0  0 NA  1 NA  1  0  0  0  0 NA  0 NA  0  0  0 NA NA  0  0  1 NA
#>  [9625]  0  0 NA  0 NA NA NA  0  0  0  0  0  0 NA  0 NA NA  0 NA  0 NA  0 NA NA
#>  [9649] NA  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  1  0  0  0  1  0
#>  [9673]  0  1  0  0  0 NA  0 NA  0  0 NA  0 NA NA  0 NA  0 NA NA  0  1  0  1 NA
#>  [9697] NA NA NA NA  1  0 NA NA  1  0  1  0  0  0  0 NA  0 NA  0 NA  0  0 NA  0
#>  [9721]  0  0  0  0  0  0 NA  0  0 NA NA NA  0  0  0 NA NA NA NA NA  0  1  0 NA
#>  [9745]  1  0 NA  0  0  0  1  0 NA  0  0 NA  0  0  0  0 NA  0 NA  0  0  0  0  0
#>  [9769]  0 NA NA  0  0  0  0 NA  0 NA  0  0 NA  0 NA  0 NA  1  0  0  0  0  1 NA
#>  [9793]  0 NA  0  0 NA  0  1  1  0  1  1  0  0  0 NA  0  0 NA  1  0  0  0  0  0
#>  [9817]  0  0  0  0  1  0  0 NA  0 NA  0  0  0  1 NA NA  0 NA  0 NA  0  0  0  0
#>  [9841]  0  0 NA  0  0 NA  0  0  0  0  0 NA  0  0 NA  0  0  1  0  0  0  0  0 NA
#>  [9865] NA  0 NA NA  0  0  1  0  0 NA  0  0  0  0  1  1  0  0  1  1 NA NA  0 NA
#>  [9889]  0  0  0  1  0  0 NA  0  0  0  0 NA NA  1  0  0  1 NA NA  1  0 NA  0  0
#>  [9913] NA  0  0 NA NA  0 NA  0 NA NA  0  0  0 NA NA NA NA  0  0  1  0  0  0  0
#>  [9937] NA  0 NA  0 NA NA  0 NA  0  0  0  0  0 NA NA  0  0 NA  0 NA  0  0  0 NA
#>  [9961]  0  0  0  0 NA  0  0  0  0  0  0  0  0 NA NA  0 NA  0 NA NA  0 NA  0  0
#>  [9985]  0 NA NA NA  0 NA  0  0 NA  0 NA  0 NA  0  0 NA  0  0  0  0  0  0  0  0
#> [10009]  0  0  0  0 NA  1  0  0 NA  1 NA  0 NA NA  1 NA NA  1  0 NA NA  0  1 NA
#> [10033] NA NA  0  0  0  0  0  0  1  0  0  1  1  0  0  0  0  0  0  1  0  0  1  0
#> [10057] NA  0  0  0  0  0 NA NA  0 NA NA NA NA NA  0  0  0 NA  0 NA NA  0 NA  0
#> [10081]  0  0  0  0  0 NA NA NA  0 NA  0 NA NA  0 NA  0  0  0 NA  0  0 NA  0 NA
#> [10105]  0  0  0 NA  0 NA NA NA NA  0  0  1 NA NA  0  0  1  1  1  0  1  1 NA NA
#> [10129]  0 NA NA  1  1 NA NA  0 NA  0  0  1 NA  0 NA  1  0 NA  0  1 NA  0  0  1
#> [10153]  0  0  0  0  0 NA  0  0  0 NA  1  0 NA NA  0 NA  1 NA  0  1  0 NA NA  1
#> [10177]  1  0  0 NA  0  1  0 NA NA  1  0  0 NA NA  0  0  1 NA NA NA  0  0  0  0
#> [10201]  0  0  0 NA NA NA  0  0 NA  1  1 NA  0 NA  0  0  1  0  1  0  0  0  0 NA
#> [10225]  0  0  0  0  0  0 NA  0  1  0  0  0  1  1 NA  0  0  0  0  1  1  1  0  0
#> [10249]  0  0  0  0  0  0  0  0  1  0  0  0  0 NA  0  0  1  0  0 NA NA  0  0 NA
#> [10273]  0  1  0  0  0  0 NA  0  1  0  1 NA  0  1 NA  0  1  0  0  0  0  0  0  0
#> [10297]  0  0  0  0  0  0  0  0  0  0  0  1  1  0  0  0  1  1 NA  1  0  0  0  0
#> [10321]  0  0 NA  1  0  0  0  0  1  0  0 NA  1  1  0  0 NA  0  1  0  0  0  0  0
#> [10345]  0  0  0  0  0  0  0  0  0  0  0  0 NA NA  0  0  0 NA  0 NA  0 NA  1  0
#> [10369] NA  1 NA  0  0 NA  0  0 NA  1  0 NA NA  1  0  0  0  0  0 NA  0 NA  0  1
#> [10393]  0 NA NA NA NA  0  0  0  1  0  0  0 NA  0 NA  1  0  1  0  0 NA  0  0  0
#> [10417]  0 NA NA NA NA NA NA  0 NA  0 NA  0  0  0  0  0  0  1 NA  0  0  0  0  0
#> [10441]  0  0  0  0  0  0  0  0  0  0  0  1 NA NA  0 NA  0  0 NA  0  0  0  0  0
#> [10465] NA  1  0  0  0 NA  0  1  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0
#> [10489]  0 NA  0  0  1 NA  0 NA  0  0  0  0  0  0  1  0  0 NA  0  0  0  0  0  0
#> [10513]  0  0  0 NA  0 NA  0 NA  0  0 NA  0 NA NA NA  0 NA  0  0  0 NA  0  0  0
#> [10537]  0 NA  0 NA NA  1 NA NA NA  0 NA  0 NA  0 NA  1  0 NA  0 NA  0  0  0 NA
#> [10561]  1  0  1  0  1  0  0  0  0  1  1  0  0  0  0  1  0  0  0  0  0  0  0  0
#> [10585]  0  1  0 NA  0  0  0  0  0 NA  0  1  0  0  0  0  0  1  0  0  0  0  0  1
#> [10609]  0  0  0  0  0  1 NA NA NA NA NA  0  0  0  0  1  0 NA  0  0 NA  0 NA  1
#> [10633]  0  0  0  1  0  0 NA NA  0 NA NA NA NA  0 NA  1  0  0  0  0 NA  0  0  0
#> [10657] NA NA NA NA  0  0  0 NA  0  0  0 NA  0  0  1  0 NA  0  1 NA  0 NA NA NA
#> [10681]  0  0 NA NA NA  0 NA  0  0 NA NA NA NA  1 NA  1  0  1  0 NA NA NA  0 NA
#> [10705]  1 NA  0  0  0  0 NA  0  0  0  0 NA NA  0  0 NA  0 NA NA NA NA NA  0 NA
#> [10729] NA  0  0  0 NA NA  0 NA NA  1  0 NA  1  1 NA  1 NA NA NA NA NA NA NA  0
#> [10753] NA  0 NA  0  0 NA  0 NA NA NA NA NA NA NA NA  0  0 NA  0 NA NA NA  0  0
#> [10777]  1  0  0 NA NA  0 NA NA  0  0  0  1 NA NA NA NA NA NA NA NA  0  0  0  0
#> [10801]  0  1  0  0  0  0  0  0  0 NA  0  0 NA  0 NA  0  0  0  0  1  1  0  0  1
#> [10825]  0  0  1  0  0  0  1  0  1  0  0  1  0  1 NA  0  0  1  0 NA  0  0 NA  0
#> [10849]  0 NA  1  1  0 NA  1  1  0 NA  0 NA NA  0 NA  0  0 NA  1  0  0  0 NA  1
#> [10873]  1  1  0  0  0  0  0  1  1  0  1  1 NA  0  0 NA  0  0  0  0  0  0  0  1
#> [10897] NA  0  0 NA  0 NA  0  0  0  1  0  1  1  0 NA NA  0  0  0  0 NA  0 NA  0
#> [10921]  0  0  1  0  1  1  0  0  0  0  0 NA  1 NA  0  0 NA  1  0 NA  0  0  1 NA
#> [10945] NA  0 NA  0 NA NA  0  1  0  0 NA NA  1  0  0  0  0 NA  0  0  1  0  0  1
#> [10969]  0  0  0  0  0  0  0  0 NA  0  0  0  0 NA  0 NA NA  0 NA  0  1  0  0  0
#> [10993]  0 NA  0  1  0  0 NA NA  0 NA  0  0 NA  0  0  0 NA  0  0  0 NA  0  1  0
#> [11017]  0  0  1  0  0  0  1  0  0  0 NA  1  0  0  0  1 NA  1  1  1  1  0 NA NA
#> [11041] NA  0  0  1  0  0  0  0  0  1  0  0  0  1  0  0  0  1 NA  0  0 NA NA  0
#> [11065]  1  1 NA NA NA NA  1  1 NA  0  0  0  1 NA NA  0  1 NA  0  0 NA  1  0  0
#> [11089] NA NA  0 NA NA NA  1  1 NA  1  1  1  0  0  1  0 NA  0  0  0  0  1  0  0
#> [11113]  0  0 NA  0  0  1  1  1  0 NA NA NA  0  1  1  0 NA  0 NA NA  1  0  0  0
#> [11137] NA  1  0  0 NA NA  0 NA  1  0  0 NA  0  0  0  0 NA NA NA  0  0  1  0  0
#> [11161]  0  0 NA  0  1  1  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1
#> [11185]  1  1  0  0  0  0 NA  0 NA  0  1 NA  1  0 NA  1  1  1  0  0  0  0 NA  1
#> [11209]  1  0 NA  0  0  0 NA  1  0  1  1  0  1  1  0  1  0  0  1  0 NA NA NA  0
#> [11233]  0  0  0  1  0 NA  0  0  0 NA  0  0 NA NA  0 NA  0  1  0  0 NA  0 NA  0
#> [11257]  1  0  0  1  0 NA  0  0 NA  0 NA  0  1 NA  0  0 NA  0  1  1 NA NA NA NA
#> [11281] NA  0  0  1  0  0 NA NA NA NA NA  0 NA NA  1  0  0  0 NA  0 NA  1  1 NA
#> [11305]  0  0 NA  1  0  1 NA NA NA  1 NA  0  0  1  0  1 NA NA  0 NA  1  1  1 NA
#> [11329] NA  0 NA  1  0  0  0 NA  0  0  0  0  0  0  1  0 NA NA  1  0  1  0  0  0
#> [11353]  0  0 NA  0 NA  1  1  0 NA  0  0  0  0  1  0 NA  0 NA  0  0  0  1  1  0
#> [11377]  0  0  1  1  0 NA  0  0  1  1  0  0  1  0  0 NA  0  0  0 NA  0  0  0  0
#> [11401] NA NA NA  0 NA  0  0  0  0 NA  0  0  0  0  0  1  0  0  0  1  1  1  0  1
#> [11425] NA  0  1  0 NA NA  0  1  0  1 NA NA  1  1  0  0  0  0 NA  0  1  0  0  0
#> [11449] NA  1  0 NA NA NA NA NA NA NA NA NA NA NA NA NA NA  0 NA NA NA NA NA NA
#> [11473] NA NA NA NA NA NA  0 NA NA NA NA NA NA NA NA NA NA NA NA  0  0  0  0  0
#> [11497]  0  0 NA  0  0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1  0  0
#> [11521]  0  0  0  0  1 NA  0  0  0  0  0 NA  1  0 NA NA  0  0  0 NA  0 NA  0  1
#> [11545]  1  0  0  0  1  1  1  0  0  0 NA  0  0  0 NA  0  0  0  0  0  1  0 NA  0
#> [11569]  0  0  0  0  0  0  0  0  0  0  0  1  0  1  0  0  0 NA  0  0  0  0  0  0
#> [11593]  0  0  0  0  0  0  0  1  0  0  0  0  1  1  0  1  1  1  0  1  0  1  0  0
#> [11617]  0  1  1  0  0  0  0  1  0  0  0  0  0  0  0  0  0  1  0  0  1  0  1  0
#> [11641]  0  0  0  1  0  0  1  1  1  0  0  0  0  1  0  0  0  0  0  0  0  1  0  0
#> [11665]  0  0 NA  0  0  1  0  1  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  1
#> [11689]  0  0  0  0  1  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  1  0  0
#> [11713]  0  1  1  0  1 NA  0  1  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0
#> [11737]  0  0  1  0  0  0  0  0  1  0  0  1  0  0  1  0  0  0  1  0  0  0  0  0
#> [11761]  0  0  0  1  1  0  1  1  0  0  0  0  0  0  1  0  0  0  0  0  1  0  1  0
#> [11785]  0  1  0  0  0  0  1  0  0  0  1  0  0  0  0 NA  0  0  0  0  1  0  0  0
#> [11809]  0  0  0  0  0 NA  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  1
#> [11833]  0 NA  0  0  0  0  0  0  0 NA  0  1  0  0  0  0  0  0  0  0  1  0  1  0
#> [11857]  0  0  0  0  0  0  0 NA  0  0  1  0  0  0  0  1  0  0  0  0  1  0  1  0
#> [11881]  0  0  0  1  0  0  1  0  0 NA  0  1  1  1  1  1  0  0  1  0  0  0  0  1
#> [11905]  1  0  1  1  0  1  0 NA  0  1  0  0  1  0  0  0  1  0  0  0  0  0  0  0
#> [11929]  0  0  1  0  1  0  0  0  0  0  1  0  1  1  1  0  0  0  0  0  0  0  0  0
#> [11953]  1  0 NA  0  0  0  0  0  0  0 NA  1  0  0  0  0  1  0  0  1  0  0  0  0
#> [11977]  0  1  0  0  0  0  0  1  0  0  0  0  1  0  0  0  0 NA  0  0  0  0 NA  1
#> [12001]  0 NA  0  0  0  0 NA  0 NA  0  0  0  0  0  0  1  0  0  0  0  0  1  0  0
#> [12025]  0  0  0  0  0  0  0  1  0  1  0  0  0  0  0  1  1  0  1  0 NA  0  0  0
#> [12049]  0  0  0 NA  0  0  0  0  0  1  0  0  1  1  0  0  1  0  0  0  0  0  0  0
#> [12073]  0  0  0  0  0  0  0  0  0  1  1  0  0 NA  0  0  0  1  0  1  1  0  0  0
#> [12097]  1  1  0  0  0  0  0  0  0  1  1  1  1  0  0  1  0  0  1  0  0  1  0  0
#> [12121]  0  0  0  0  0  0 NA NA  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0
#> [12145]  0  0  0  0  0 NA  1  1  1  0  0  0  1  0 NA  1  0  1  1  0  0  0  1  0
#> [12169]  0  0  0  0 NA  0  0  0  0 NA  0  0 NA  0  0 NA  0  1  0 NA  0  0  1  0
#> [12193]  0  0  0  0  0  0  1  0 NA NA  0  0  0 NA  0  0  0  0  0  0  0  0  0  0
#> [12217]  0  0  0  0  0 NA NA  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0
#> [12241]  0  0  1  0  1  0  0  0  0 NA  1  0  0  0  0  0  0  0  0  0  0  0  0  0
#> [12265]  0  0  0  0 NA  1  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0
#> [12289]  0  1  0  0  1  0  0  1  1  1  0  0  0  0  1  0  0  0  1  1  0  0  0  0
#> [12313]  1  0  0  1  1  1  1  1  0  0  0  0  0  1  0  0  1  0  0  0  1  0  0  0
#> [12337]  0  0  0  0  0  1  1  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  1  0
#> [12361]  0  1  1  0  0  0  1  1  0  1  0  1  1  1  1  0  1  1  0  0  0  1  0  0
#> [12385]  0  0  0  1  1  0  0  0  1  0  1  0  0  0  0  1  0  0  1  1  0  1  1  1
#> [12409]  1  1  0  1  0  1  0  0  1  0  0  0  1  1  1  0  0  1  1  0  0  1  0  1
#> [12433]  1  0  1  1  0  0  0  1  0  1  1  1  0  0  0  1  1  1  0  0  1  1  1  1
#> [12457]  0  1  0  0  0  0  1  0  1  0  0  0  1  0  0  0  0  1  0  0  0  0  1  1
#> [12481]  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0 NA NA  0 NA  0  1 NA  0  1
#> [12505]  0  0  0  0  0  1  1 NA  0  1  0  0  0  1  0  0  0  1  0  0  0  0  1  0
#> [12529]  0  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0  0
#> [12553]  0  0  0  0  0  0  0  0  0  1  0  1  1  1  0  0  0  0  0  0  0  0  0  0
#> [12577]  1  0  1  0  1  1  1  1  1  1  1  0  1  1  1  1  0  0  1  0  0  0  0  0
#> [12601]  0  0  1  0  0  1  0  0  0  0  0  1  1  0  1  0  0  1  1  1  0  1  0  0
#> [12625]  0  0  1  0  1  0  1  0  1  1  0  0  0  0  0  1  1  0  0  1  0  1  1  1
#> [12649]  0  0  1  1  1  0  0  1  0  0  0 NA  1  1  1  0  1  0  0  1  1  0  1  0
#> [12673]  1  1  0  0  1  0  1  0  1  1  0  1  0  1  1  0  1  1  0  0  1  1  1  0
#> [12697]  0  1 NA  0  0  1  0  1  0  1  0  0  0  0  1  0  0  0  0  0  0 NA  0  0
#> [12721] NA  0  0  0  0 NA  0  0  0 NA  0 NA  0  0  0  0  0  0  0  1  1 NA  1  1
#> [12745]  1  1  1  1  1 NA  1  1  1  1  1  0  1 NA  1  1  1  1  1  1 NA  1  1  1
#> [12769]  1  1  1  1  1 NA  0  1 NA  0  0  0 NA  1  0 NA  0  0  0  0  0  1 NA  0
#> [12793]  0  0  0  0  0 NA  1  0  0  1  1  0 NA  0 NA  0 NA  0  0  0  0 NA  0 NA
#> [12817]  1  0  0 NA  0  0  0 NA  1  1 NA  0  0  0 NA  0 NA  0  0 NA NA  0 NA  1
#> [12841] NA  0  0  0  0  1  0 NA  0  0  0  0 NA  1  0  1  0  0  1  0  0  0  0  0
#> [12865]  0  0  0  0  1 NA NA  0  0  0  0  0  0  0 NA  0 NA  0  0 NA  0 NA NA  0
#> [12889]  0  1  1  0 NA  0 NA  1  0 NA  0  1  0 NA  0  0 NA  1 NA  0  0  0  0  0
#> [12913]  0  0  1 NA NA NA  0  0  0 NA  0 NA  0 NA  0 NA NA  0  0  0  0 NA NA NA
#> [12937] NA NA NA  0  0  0  0  0  0 NA NA  0  0  0 NA  0  0  0 NA NA NA  1  1  0
#> [12961] NA NA NA NA  0  0 NA  1 NA  0  0 NA  1 NA  0 NA NA  0  0 NA  0  0 NA NA
#> [12985] NA  0  0  0  0  0  0  0  0  0 NA  0 NA  1  0 NA  0  0 NA  0 NA  0  0 NA
#> [13009] NA NA NA NA  1  0  1  0  1  0 NA  0  1  0  0  0  0  0  0 NA  0  0  0  0
#> [13033]  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  0 NA NA NA NA  0  0  0  0
#> [13057]  0  0  0  0  0  0 NA NA  0 NA NA NA  0 NA NA NA NA  0  0  0  0 NA  0  0
#> [13081] NA NA  0  0  0  0  1  0 NA  0  0  0  1  0 NA  0  0  0  0  0  0 NA  0  0
#> [13105] NA  0 NA  0  0  0 NA  0  0  0  0  0 NA  0  0  0  0  0 NA  1  0  0  1  0
#> [13129]  0  0  0  0 NA  0  0  0  0  0  0  0  1  0  1  0  0 NA  0  0 NA  0  0  0
#> [13153]  0 NA  0  0  0 NA  0  0  0  1 NA  0 NA  0  0  0  0  0  0  1  0 NA  0  0
#> [13177]  0  0  0  0  0  1  0  0  0  0  0  0  0  0 NA  0  0  0  0  0  0 NA  0 NA
#> [13201]  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> [13225]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA NA  0 NA  0 NA NA NA
#> [13249]  0 NA  0  0  0  0  0 NA NA  0  0 NA  0 NA  0  0  1  0 NA NA  0  0 NA  0
#> [13273]  0  0  0  0 NA  1 NA  0 NA NA  0  0  0 NA  0  0 NA  1  0 NA  0  0  0  0
#> [13297]  0 NA  0  0 NA NA  0 NA  0  0  0  0 NA  0 NA  0  0  1 NA  0  0  0  0 NA
#> [13321]  0  1  0  0  0  0 NA  0  0  1 NA  0  0  0  0 NA  0  0  0 NA  0  1  0  0
#> [13345]  0  0 NA  0  1  0  0 NA  1  0 NA  0  0  0 NA  0  0  0 NA  0  0  1  0  0
#> [13369]  0  0  0  0  0 NA  0 NA  0  0  0  0  0  0  0 NA  0  0  0 NA NA  0  0  0
#> [13393]  0  1  0  0  0  0 NA NA  0 NA  1  0  0  0  0  1  0 NA NA  0 NA  1  0  0
#> [13417]  0  0  0  0 NA NA  1  0  0  0 NA  0  0  0  0  0 NA  1  0  0  0  0  0  0
#> [13441]  0  0 NA  0  1  0  0  0 NA  0  0  0 NA  1  0  0  0  0  0  1  0  0  0  0
#> [13465]  0  0  0  1 NA  0  1  0  0 NA  0  0  0  0  0  0  0  0  0  0  0  0  0  0
#> [13489]  1  0  0  0  1  0  0  0  0  0  0  1  0  0  0  1  0  0  0  0  0  0  0 NA
#> [13513]  0  0  0  0  0  0  0 NA  0 NA  0 NA  0  0 NA  1  0  0  0  0  0  0  0  0
#> [13537]  1  0  0 NA  0  0  0  0 NA  0  0 NA  0 NA  0  1  0  0  0  0  0  0 NA  0
#> [13561]  0  1  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0 NA  0  0  0  0  0 NA
#> [13585] NA  0  0  0  0  1  0  0 NA  1 NA NA  0  1  0  1  1  1  1  1 NA NA  0 NA
#> [13609]  1 NA  1  1  1  1  1  1 NA  0 NA  0  0  1 NA  0  0  1 NA NA  1  1  1 NA
#> [13633]  1  0  1  0  1  1  0 NA NA  1 NA  0  1  0  0 NA  1  0  1  1 NA  0 NA  0
#> [13657] NA  1 NA  0  0  1 NA NA  0 NA  0 NA  1  0  1 NA  1  0  0  0  0 NA  0  0
#> [13681] NA NA NA  0 NA NA  1 NA NA NA  0 NA  0  1  0 NA NA  1  1  1 NA  1 NA NA
#> [13705]  1  1 NA  0  1  0  1  0  1  0 NA  0 NA  1 NA  1  1 NA NA  1  1 NA NA  0
#> [13729] NA  1 NA  0  1  0  0  1  1 NA  1  1  1 NA  0  1  0  0  0 NA  0  1  1  1
#> [13753] NA NA  1  1 NA NA  0 NA NA NA  1 NA  1  1  0  1 NA NA  1  0  0  1 NA NA
#> [13777] NA NA NA NA  1 NA  0 NA  1  1  1  1  1 NA NA  0 NA NA  1 NA  0  0 NA NA
#> [13801]  1  1 NA  0  1  1  1 NA NA NA  1  1  0  0  1  1  0  1  0 NA  1  1  1  0
#> [13825]  1  0  0  1  0  1  0 NA NA NA  1 NA  1  0  1  0  0  1  0  0  1  0  0  1
#> [13849]  0  1  0 NA NA NA  0  0 NA NA  0  1 NA  0  1 NA NA  0  1  1  1 NA  1  0
#> [13873] NA  1  1  1  0  0  1 NA NA NA  0 NA  0  0  1 NA NA  0  1  0  0  1 NA NA
#> [13897] NA  0  1  1  1  0  0  1  1 NA  0  0  1 NA  0  1 NA  0  1  0  0 NA NA  1
#> [13921] NA  1  0  0 NA NA  0 NA NA  0 NA  0  1  0  1  0 NA NA NA  0  1 NA  0 NA
#> [13945]  0  0 NA  0 NA  0  0 NA NA  1  0  1 NA NA  1  1  1  0  0  1 NA  1  1  1
#> [13969]  0  1 NA  0  0 NA NA  1  0  0 NA NA  0  1 NA NA  0 NA  1  0  0  1  1  1
#> [13993]  0  1  0  1  0  1  1 NA  1 NA  0  0 NA  1  0  0  0  1 NA NA  0  1  1  0
#> [14017]  0  1  0  1 NA  0  1  0 NA NA  1  1  0  1 NA  1  1  1  1  1  0  0 NA  0
#> [14041] NA  1  0  0 NA NA  1  1  1  1  0 NA NA NA  0  1  0 NA  1  1 NA  0  1  0
#> [14065]  1  1  0  1 NA  1  1  1  1  1  0  0  0  0  1  0 NA NA NA NA  0 NA  1  1
#> [14089]  1  1  0  1  1  0  0  1  0  0 NA NA  0  0  1  1  1  1 NA  0  0  0  0  1
#> [14113]  1  1  0  1  1 NA  0  0  0 NA  0  0  1  1  1  1  1  1  1  1  0  1 NA  0
#> [14137]  0  1  1  1  0  0  0 NA  1  0  0 NA NA NA NA NA  1  0 NA  0  0  0  0  0
#> [14161]  1  0  1 NA  0  0  1  0 NA NA  0  0  0 NA  0  0  1  0 NA  1  1  0 NA  1
#> [14185] NA  1  0  0  0  1  0 NA NA NA NA  1 NA  0  1  0  1  1  1 NA  0  0  0  0
#> [14209]  1  1  0 NA NA  1  1  1  0  1 NA NA  1  0  0  0  0  0 NA  1 NA NA  1  0
#> [14233]  1 NA NA  0  1 NA  1  1  1 NA  1  0  0  1 NA  0 NA  0  1  0 NA  0  1  0
#> [14257]  0  0  0  1 NA  0  1 NA  1  0  0 NA  1  0 NA NA NA  1  0  0  1  0 NA  1
#> [14281] NA  1  0  1 NA NA  0  0  0 NA  1  1  0  1  0  0 NA NA NA  0  1  0  1  0
#> [14305]  0  1  1  1  0  1  1  1  0  1  0  0  1  0  0  0 NA NA  1  1  1  1  1  0
#> [14329]  1  1 NA  1 NA NA  1  1  0  1  1  0  0  0  0  1  0  0  0  0  0  0  1  0
#> [14353]  0  0  0  1  0  1  1  0  1  1  0  0  1  0  0  0  0  0  1  1  1  0  0  0
#> [14377]  0  0  0  0  1  0  0  0  0  0  0  1  0  1  0  0  0  0  0  0  0  1  0  0
#> [14401]  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  0  0  1  0  0  1  0  0
#> [14425]  0 NA  0  0  0  0  0  1  0  0  0  0  0  1  0 NA  0  0  0  0 NA  0  0  0
#> [14449]  0  1  0  0  0  0  1  0  0  0  0  0 NA  0  0  0  0  1  0  0  1  0 NA  0
#> [14473]  0 NA NA  0  0  0  0  0 NA  0 NA  1  0 NA NA NA  0  1 NA NA NA NA  0 NA
#> [14497]  0  0  0  1 NA  0 NA  0  0  0  1  1  0  0  0  0 NA NA  0  0  1  0  0  0
#> [14521]  0  0 NA  0  0  0  0 NA  0  0  0  0  0 NA  1  0  0  0 NA NA NA NA NA  0
#> [14545] NA NA NA NA NA  0 NA  1 NA  0 NA  0  1 NA NA NA  0  0  0  0  0 NA NA  1
#> [14569] NA  0  0 NA NA NA NA NA NA  0  0 NA  0  1  0 NA  0  0  0  1  0  0 NA  0
#> [14593] NA NA  0  0  0  0  0  0 NA  0  0 NA  0 NA NA  1  0  0 NA  0 NA  0  1  0
#> [14617]  0  0 NA  0  0 NA  0 NA  0 NA NA  0  0  0  1  1  1  0  0  0  0 NA  0  0
#> [14641]  1  0  0  1  0  0  0 NA  1 NA  0  0 NA NA  0 NA NA  1  0  0  0 NA  0 NA
#> [14665] NA NA  0 NA  0 NA  0  0 NA  0  1  0  0  0  0  0 NA  0  0 NA  0 NA NA  0
#> [14689] NA  0  0  0 NA  0  0  0  0 NA NA  0  0  0 NA NA  0 NA  1  0  0  0 NA  0
#> [14713]  0  0  0  0  0  0  0  0  1  0 NA NA  1  0 NA  0 NA  0  0  0  0  1  0 NA
#> [14737]  0  1  0  0 NA  0 NA NA  0 NA  1 NA NA  1  0  0  0  0  0  1  1  0 NA  0
#> [14761]  0 NA  0  0  0  0  0  0  0  0  0  0 NA NA  1  0  0  0  0  0  0 NA  0  0
#> [14785]  0  1  0 NA  0  0  1  0  1  0  1  0  0  1  0  0  0  0 NA  0  1  0 NA  0
#> [14809]  0  0  1  0  0  0  0  0 NA NA  1  1  1 NA  1  1  0  1 NA NA NA NA  0 NA
#> [14833]  1  0  0  0 NA  1  0  0 NA  0 NA NA NA NA NA NA  1  0  0  1  1  1  1 NA
#> [14857] NA  0 NA  0 NA  0  1  1  1  0 NA NA NA NA  0  0 NA NA  0 NA  1  0 NA  0
#> [14881]  0  0  0 NA  0 NA NA  0 NA  0 NA  0 NA  0  0 NA NA NA  0  0  0  0 NA  0
#> [14905]  0  0  0  0 NA  0 NA NA NA  1  1  0 NA  0  0 NA  0 NA NA  0 NA  0  0  0
#> [14929]  0  0  0  0  1  0 NA  0  1  1  0  0 NA NA NA  0 NA NA  1  0  0  1  0  0
#> [14953] NA  0 NA  0 NA  0 NA  0  1  0 NA  0  0  1 NA  1  1  0  0  0  1  0  0  0
#> [14977]  1  0  0  0  0  0  0  0  0 NA  1 NA  0  0  0  1 NA NA  0  0  0  0  0  0
#> [15001] NA  1  0  0  0  0 NA  0  0  1 NA NA  0  0  0  0  0  0  0  0  1 NA NA NA
#> [15025]  1 NA  0  0 NA  1  0  1 NA NA  0  0 NA NA NA NA  0  0  1  0 NA NA NA NA
#> [15049]  1 NA NA  0 NA  0 NA NA NA NA NA  0  0 NA  0  0  0  0  0  1  0 NA  0  0
#> [15073]  0  0  0  0  0  1  0  0  1 NA NA  1  0  0  0  0  0  1  0  0  0  1  1 NA
#> [15097]  1  0 NA  0  0 NA  0  0  1  0  0  0  0  1  1  0  0  0  0  0  1  1  0  0
#> [15121]  1  0  0  0  0  0 NA NA  0  0  1  1 NA  0  0  0  1  1  0  1  1 NA  0  1
#> [15145] NA  0  0  1  1 NA NA  0  0  0  0  1  0  0  0  0  0 NA  0  0  0  0  1  0
#> [15169]  1  0  1  1  0  1  1  0  0  0  0  0  0  1  0  0  0  0  0  0  0  0  0  0
#> [15193]  1  0  0 NA  1  0  0 NA  0  0  0  0  0  0  1  0  0 NA  0  0  1  0  0  0
#> [15217]  0  0  0  0  0  1  0 NA  0  0  0  0  1  0  0  0  0 NA  0  0 NA NA NA  0
#> [15241]  0  0  0 NA  0  1  0  0  0  0  0  0  1 NA  0  0  0 NA  0  0  0  0  0  0
#> [15265]  1  0  1  0 NA  0  0  0  0  0  1  0  1  0  1  0  0  0  0 NA NA  0  0  0
#> [15289] NA  0  0  0  0  0  0  0  0  0  0  0  1  0  0  0  0  1  0  0  0  0  1  0
#> [15313]  0  1  0  1  1  0 NA NA  0  0  0  0  1  0  0  1  0  0 NA  0  0  1  1  0
#> [15337]  1  0  1  1  1  0  0  1  0  1  1  0  1  1  1  1  1  0  0  0 NA  1  0  1
#> [15361]  0  0 NA  1  0  0 NA  1  0  0  0  0 NA  0 NA  0  1  0  1 NA  1  0  0  1
#> [15385]  0  0  1  0  0  1  1  0  1  1  0  1  1  0  0  0  0  0  1  0  0 NA  0  0
#> [15409]  0 NA NA  0 NA NA  0  0  0 NA  1  0  0  1 NA NA NA  0  0  0  0 NA  1 NA
#> [15433]  0  0  0  0  0  0  1  1  0  1  1 NA NA  0  0 NA  0  0  1  0  1  0  0  0
#> [15457]  0  0  1  0 NA  1  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0 NA NA  0
#> [15481]  0 NA  0  1  0 NA  1 NA  0 NA NA  0  0  0  0 NA  0  0  0  0  0  0  0  0
#> [15505]  0  0  0 NA  0  0  0  0  0  0 NA NA  0  0  0 NA  0  0 NA  0  0  1  0  0
#> [15529]  0  0  0  1  0 NA  0  0  0  0  0  0  0  0 NA  0  0 NA  0  0  1  0  0  0
#> [15553]  0 NA NA  0  0  0  0  0  0  0  0  0  0  0  0  0 NA  0  1 NA  0 NA  0  0
#> [15577] NA  0  0  0  1  0  0  0  1  0  0  0 NA NA  0  0  0 NA  0  0  0 NA NA NA
#> [15601] NA  0 NA  0  0  0  1 NA  0  0 NA  1 NA  0  0 NA  0  0  1  0 NA  1 NA  0
#> [15625]  0 NA NA NA  0  1  0 NA  1  1 NA  0  1 NA  1  0 NA NA NA NA  0  0  0  1
#> [15649] NA  0  1  1  1 NA  1  0  0  1  1  1  0 NA NA  0 NA  1  1  0  1  0  1  1
#> [15673] NA  0  0 NA  0  0  0 NA  1  1  0  0  0  1  0  0  0  1 NA NA  1  0  1 NA
#> [15697]  1  1  1  0  0  1 NA NA  0 NA  0  1  0 NA  1 NA  1  0  0 NA NA  1  0  0
#> [15721] NA  1 NA  0  0  0 NA  0  0  0 NA NA  1  0  1 NA  0 NA NA  1  1  0 NA NA
#> [15745]  0  0 NA  0  1  1  1  1  1  0  0  0  1  1  0  0  1 NA NA NA NA  0 NA  0
#> [15769]  1  1 NA  0 NA  1 NA  0  1 NA  1 NA  1  0 NA NA NA NA  1  1  0 NA  0  1
#> [15793] NA  1  1  1 NA  0  0  1 NA  0  1 NA  0 NA NA  0  1  1  0  0  0 NA  0 NA
#> [15817]  1  1  1 NA  1  0  1  1  1  1  1  0 NA  1  1  1  0  1  1  1  0  1 NA  0
#> [15841] NA  0  1  0  1  1  0  0 NA  1  0  1  0  0  1  0 NA  0  0  0  1  0  0 NA
#> [15865]  1  1 NA NA  1  0  0 NA NA NA  1 NA NA  0  1  0  1  0  0  0  0  1  0  0
#> [15889]  0  0  0 NA NA  0  1 NA  0 NA  1 NA  1  1 NA NA  1 NA NA  0 NA  1 NA  1
#> [15913]  1 NA  0 NA NA  1 NA  0  1 NA  1  1  1 NA  1  0  1 NA  0  0 NA  1 NA  1
#> [15937]  1 NA NA  1  1  0  0 NA  1  1 NA NA  0 NA  0 NA NA NA NA NA NA  1 NA NA
#> [15961] NA NA  1  1 NA  1 NA  1 NA  1  1  0  1  1  0  0  1 NA  1  0  0  1  1  1
#> [15985]  1  1  1  1  0 NA  1 NA  0  0 NA NA  0  1  1  0  0  1  0 NA  0 NA  1  0
#> [16009]  0  0 NA  0  1  0  1  1  1  1 NA  1  0 NA  1  1  1  1  0  1  1  0  1  1
#> [16033]  0  1  1  0  0  1  0  0  1  0  1 NA  1  1  1  0  0  0  0  1  0  1  1  1
#> [16057]  0  0  1  1  0  1  0  1  1  1  0  1  1  0  1  0  0  1  1  1  1  0  0  0
#> [16081]  1  0  1  1  0  0  0  0  0  0  0  0  0  1  0  1  0  0  0  0  1  0  0 NA
#> [16105]  0  0  1  0  0  0  1  0  0  0  1  1  0  1  0  1  1  0  0  0  1  0  1  0
#> [16129]  0  0  0  0  1  0  1  1  1  0  1  0  0  0  0  0  0  0  1  1  1  1  0  1
#> [16153]  1  0  0  0  0  0  0  1  0  0  0  1  0  0  1  1  1  1  0  0  1  1  0  1
#> [16177]  0  1  0  1  0  0  1  1  1  0  1  1  0  1 NA  0  1  0  1  0  1  1  1  0
#> [16201]  0  0  0  1  1  1  1  0  1  1  0  1  0  1  1  0  0  1  0  0  1  1  0  1
#> [16225]  0  1  0  1  1  1  1  1  1  0  0  1  1 NA  0  0  0  1  0  1 NA  1  0  0
#> [16249]  1  1  1  1  1  0  0  0  0  1  1  0  0  0  1  1  1  1  1  1  1  1  1  1
#> [16273]  0  1 NA  0  1  0  0  1  0  0  0  0  0  0  0  0  0  1  1  0  0  0  0  0
#> [16297]  1  0  1  1  0  0  1 NA  0  0  0  1  0  0  0  1  0  0  1  0  1  0  1  0
#> [16321]  0  1  0  0  1  1  0  0 NA  0  0  1  1  0  1  1  0  1  0  1  0  0  1  0
#> [16345]  0  0  0  0  0  0  0  0  1  0  0  0  1  0  0  1  1  0  0  1  1  1  0  0
#> [16369]  0  0  1  0  0  0  0  0  0  1  0  0  1  1  0  0  0  1  0  1  0  1  0  1
#> [16393]  0  0  0  0  1  0  0  0  0  0  0  1  1  0  1  1  1  0  0  1  0  0  0  1
#> [16417]  1  1  1  1  1  0  1  1  1  0  1  0  1  1  1  0  1  1  0  0  1  0  1  0
#> [16441]  0  0  0  0  1  1  0  1  0  0  0  0  0  0  0  0  0  0  0  1  0  1  0  1
#> [16465]  0  0  0  1  0  0  0  1  0  0  0  0  0  0  1  0  1  0  0  0  1  0  0  0
#> [16489]  1  0  0  1  0  0 NA  0  0  0  1  0  0  0  0  0  0  1  1  0  0  0  0  0
#> [16513]  1  1  0  1  0  0  0  1  0  0  0  1  1  1  0  1  1  1  1  0  1  0  0  0
#> [16537]  0 NA NA  0  0 NA  1  1 NA  0 NA  0  0  0  0  0 NA  1  0  0  0  0 NA  0
#> [16561] NA NA NA  0  0  0 NA  0  0 NA  0  0  1 NA NA NA  0  0  0  0  0  0  0  1
#> [16585] NA  0  0  0 NA NA  0  0 NA  0  0  0  0  0 NA  0  1  0  0  0 NA NA  0 NA
#> [16609]  0  0 NA  0  1  0  0  0  0 NA NA NA  0  1  0  1  1 NA  1 NA NA  0  0 NA
#> [16633]  0 NA  1  0 NA  0  0  1  1  1 NA  0  0  0 NA  0  0  0  0 NA NA  0  0  0
#> [16657] NA  0  1  0  0  0 NA NA  1  0  1  0  0  0  0  0  1  0  0  0  0  0  0  0
#> [16681] NA  0  0  1  0 NA  0 NA NA  0  0  0  0  0  1  0 NA  0  0  0  0 NA  0  0
#> [16705]  0  0  0  0  0  0  1  0  0  0  0  0  1  0  1  0  0 NA NA  0 NA  0  0  0
#> [16729]  0  0  1  0  1  0  0  0  0 NA  0  0  1  1  0  1  0  0  0  0  0  0  0  0
#> [16753]  0  0  0  0  1  1  0  0  1  1  0  0  1 NA  0  0  0  0  0  0  0  0 NA  0
#> [16777]  1  0 NA NA  0 NA NA  0  0  0  0  0  0  0  0  0 NA  0 NA  0 NA  1  0  0
#> [16801]  0 NA  1  0  0  0 NA  0  0  0  0  0  0  0  1  0 NA  0  0  0  0  0  0  0
#> [16825]  0  0  0  0  0  0  0 NA  0  0  0  0  0  0  0  1  0  0  0  0  0  0  1  0
#> [16849]  0  0  0  1  0  0  0  0  1  0  0  0  0  0 NA  0  0  0  0  0  0  0  0  1
#> [16873]  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  0  1  1  0 NA  0
#> [16897]  1 NA  0 NA  0 NA  0  0 NA  1  1  0  1  1  0  1  1  1  0 NA  0  1  0 NA
#> [16921]  0  0  1  0  0  0  0  0  1  1 NA  1  1  0 NA NA  0  1  1  1  1  0  0  0
#> [16945] NA NA  1  1 NA NA NA  0  1  0  0  1  0  0  0  0  0 NA  0  0  0  1  0  0
#> [16969]  0  0  0 NA  1  0  1  0  0  0  1  1 NA  1  0  0  0 NA  0  0  1  0  0 NA
#> [16993]  0  0  0  0  1  0  0  0 NA  0 NA  0  0 NA NA  1 NA  0  1 NA  1  1  0  0
#> [17017] NA  0  0 NA  0 NA NA NA  0  0 NA NA NA NA  1 NA  0  0  1  0 NA  1  0  1
#> [17041]  1  0  0  0  1  1  1  1 NA  0  0  0  0  0  1 NA  0  0  0  1 NA  0 NA NA
#> [17065]  1 NA NA  0  1 NA  0 NA  0  0  1  0  1 NA  0 NA  0  0 NA  0  0  0  0  0
#> [17089]  0  0 NA  0  0  0  1  1  0  1  0  0  0  0  0  1 NA NA  0 NA NA  0 NA  0
#> [17113] NA  0  0  0  0  0  0 NA  0 NA  1  0  1  0 NA NA NA  0  1  0 NA NA  0  1
#> [17137]  0 NA  1  1  0  0  1  0  0 NA NA NA  0  0  0  0  0 NA  0  0  0  0  0 NA
#> [17161]  0  0  1  0 NA  1  0  0  0  0 NA  0  0  0  0  0 NA  1  1  1 NA  1  0  0
#> [17185]  0  1  0  0  1 NA NA  1  0  0 NA NA  0  0 NA  0 NA  0  0  1  1 NA  0 NA
#> [17209]  0  0  0  0  0  0 NA  1  1  0 NA NA  0  1 NA NA NA  0 NA  0  0  1  0  0
#> [17233]  0  0  0  0  0 NA NA NA  1 NA NA  0  0  0 NA  0 NA  0  0  0 NA  0  0  0
#> [17257]  1 NA  0  0  0  1 NA  0 NA  0  0  0  0  0  0  0  0  0  0  1  1  0  0 NA
#> [17281] NA  1  0  0  0  0 NA NA  0  0  0 NA  0  0  0  1  1  0 NA  1 NA  0 NA  0
#> [17305]  0  0 NA  0  0  0  0  1  0 NA  0 NA  1  1  0 NA NA NA  1  0 NA  1  0  1
#> [17329] NA  1 NA  0  1  0  1  1  0  0  0 NA NA  0  1 NA NA NA NA  1 NA NA NA  0
#> [17353]  0  0 NA  0 NA  0 NA NA NA NA NA  0  0 NA  0  0  0 NA NA  0  0 NA  0 NA
#> [17377] NA  0 NA  0 NA  0  0  0  0 NA  0  0 NA NA  0  1  0  1  0  0 NA  0  0  0
#> [17401]  1  0  0  0  0  0  0 NA NA NA NA  0 NA  0  0  0 NA  0  0 NA NA NA  0 NA
#> [17425] NA NA NA NA  0 NA  0  1  1  1  0  0 NA NA  0  0  1 NA NA  0  0  1 NA  1
#> [17449] NA  0  0 NA  0  0 NA NA  0  0  0 NA NA  0  1  1  1  1  1  1  0 NA NA NA
#> [17473] NA  1  0  0  0  0  0  1 NA NA  1  0  1  0  0  0 NA  0  0  0 NA NA NA  1
#> [17497]  0  0  0 NA  1  0 NA NA  0 NA  1 NA  0  0 NA NA NA  1 NA  0  1 NA NA  1
#> [17521]  0 NA NA  1 NA NA NA NA NA  0  0  0 NA  1 NA NA NA NA  0  1 NA  0  1  0
#> [17545] NA  0 NA NA  0  0  0  0  0  0 NA  0  0  0  0 NA  0  0  0 NA NA  0 NA  0
#> [17569]  0 NA  0  1  1 NA  0 NA NA  0 NA  0  1  0  0  1  1  0  0 NA NA  0  0  0
#> [17593]  0 NA NA  0 NA NA  0  1 NA NA NA  0 NA  0 NA NA  0 NA  0 NA  0  0  0 NA
#> [17617]  1  0  0 NA NA  0 NA  0  1 NA NA NA NA  0 NA NA  0  0 NA  1  1  0 NA NA
#> [17641]  1 NA  1 NA NA NA  0  0  1  0 NA NA  0 NA  1 NA  1 NA NA  0 NA  0 NA  0
#> [17665]  1  0  1  1  0  1  1  1  1  0  1  1  1  1  1  0  1  0  0  0  0  1  0  1
#> [17689]  1  0  0  0  1  0  1  1  1  1  0  1  1  1  1  0  1  1  1  0  1  0  0  0
#> [17713]  1  0  0 NA  0  1  0  1  1  0  0  0  0  1  0  1  1  0  0  0 NA  0  1  1
#> [17737]  0  1  0  0  1  0  1  0  0  1  0  0  0  0  0  0  0  1  1  0  0  0  0 NA
#> [17761]  0  0  0  0  1  0  1  1  0  0  1  0  0  0  0  0  0  0  0  0  0  0  0  0
#> [17785]  0  0  1  0  0  0  0  0  0  1  0  0  1  0  0  1  1  1  1  0  1  1  0  0
#> [17809]  0  1  0  1  0  0  0  1  1  1  0  0  0  0  0  0  0  0  0  0 NA  1  0  0
#> [17833]  0  1  0  0 NA  0  0  1  0  1  0  1  0  0  1 NA  0  0  0  0  0  1  0  0
#> [17857]  0  0  1  1  0  1  1  0  0  0  1  1  0  1  0 NA  0  0  0  1  0  1  0  0
#> [17881]  0  0  0  1  1  0  0  1  1  1  0  1  0  0  1  0  0  1  0  0  1  0  0  0
#> [17905]  1  0  1  0  1  0  1  0  0  0  1  0  1  0  1  0  0  0  0  0  0  0  0  1
#> [17929]  1  0  1  0  1  0  1  1  0  0  1  0  0  0  0  0 NA  1  0  1  0 NA  0  1
#> [17953]  1  0  0  1  0  0  1  0  0  0  0 NA  1  0  1  1  1  0  1  0  1  0  1  1
#> [17977]  1  0 NA NA  1  0  1 NA  0  1  0  0  0 NA  0  1  0  0  1  1  1  0  1  1
#> [18001]  1 NA  1  1  1  0  1  1  1  1  0  1  1  0  0  1  1  0  1  1  0  1  0  0
#> [18025]  0  1  0  0  0  0  0  0 NA NA  0  0  1  0  0  0 NA  1  1  1  0  0  1  1
#> [18049]  0  1  0  1  0  1  0  1  0  1  1  0  0  1  0  0  1  1  1  1  1  1  1  0
#> [18073]  0  1  1  0  1 NA  1  0  1 NA  1  0  1  1  1  1  0  0  1  1  0  0  1  0
#> [18097]  0  1  0  1  0  1  0  0  1  0  0  1  0  1  0  0  1  1  1  1  1  0  1 NA
#> [18121]  0  1  1  1 NA  1  0  0  1  1  0  0  0 NA  1  0  1  1  1  0  1 NA  0  0
#> [18145]  0  0  1  0 NA NA NA  1  0  1 NA NA  0  1  1  0  1  1 NA  0  1  0  1  1
#> [18169]  1  1 NA  0  1  1  0  1  0  0  1  1  1  1  1  1 NA  1  1  1  1  1  1 NA
#> [18193] NA  0 NA  1 NA  1  1 NA  1  0 NA  1  1 NA  1  1  1  0  1  0  0  0  1  1
#> [18217]  0  1  0  1  1  1  1  1  1  1  0  0  1  0  0  0  1  0  1  1  1  1  0  0
#> [18241]  1  1  0  1  0  1  0  1  0  0 NA  0  0 NA  1  0  0  1  0  1  0  1  1  1
#> [18265]  0  0  0  1  0  1  1 NA  0  1  1  0  0  0  1  0 NA  0  0 NA  1  0  0  1
#> [18289] NA NA  1  1 NA  0  0  0 NA NA  0  1 NA  0  1  1  1  1  1 NA  0  0  0 NA
#> [18313]  1  1  1 NA NA NA  0  1  0  1  0  1  1 NA  0  0  0  1 NA  0  0  1 NA  0
#> [18337]  0  1  1  1  1  1  0  1  0  1  1  1  0  1  1  0  0  0  0  1  0  1  1 NA
#> [18361]  0  0  1  1  0  1  0  1 NA  1  1  0  0  1  1  1  0  1  0  1  1  0  0  0
#> [18385]  1  1  1  1  0  0  0  1  1  0  1  0  1  0  0  0  1  1  1  1  0  1  1  0
#> [18409]  0  0  1  0  0  0  1  0  0  1  1  0  0  0  0 NA  1  1  0 NA  0  1  0  0
#> [18433]  1  0  0  0  1  1  0  0  1  1  0  1  0  1  1  0  0  1  1  0  1  0 NA  1
#> [18457]  0  0  1  1  0  1  0  0  1  0  0  0  1  0 NA  1  1  1  1  1  1  1  1  0
#> [18481]  1  0  1  1  1  1  1  1 NA NA  0  1 NA NA NA  1 NA  1 NA NA  1  0 NA NA
#> [18505]  0  0  1  1 NA  0  1 NA  0  0  1  0 NA  0  1  0  0  0  0  0  0 NA NA  1
#> [18529] NA  0  0  0 NA  0  0 NA  1  0  0 NA  0 NA  0  0  0  1  0  1  1  0  0  0
#> [18553]  0  0  1  0  0  0 NA NA  0  0  0  0  1  0  0  1  1  0  0  1 NA  0  0 NA
#> [18577]  0  0  0  0  0  0  0 NA NA  0  0  1  0 NA  1  0  1  0  0 NA NA  0  0  1
#> [18601]  0  1  1  0 NA  0  0  0  1  0 NA  1  1 NA  0 NA  0  1  1 NA  1 NA  1  0
#> [18625] NA  1  0  0  1  0  1  0 NA NA  0  1  0  0  1 NA  1 NA NA NA  0 NA  1 NA
#> [18649]  0  1 NA  0  0  1  1  0 NA  0  1  1  1  0  0  0  1  0  0 NA NA NA  1  0
#> [18673]  0 NA  1  0  0  0 NA  1  1 NA NA  0  0 NA  0  0  0  0  1  0  1  0  0  0
#> [18697]  1  0 NA NA  1  0  0  0  0  1  1  1  0  1 NA NA  0  0 NA  0 NA  1  0  1
#> [18721]  0 NA  1  1  1  0  1 NA  0  1  0  0  0  0  0  0  0 NA  0 NA NA  0  1 NA
#> [18745] NA  0  1 NA  0  0  1  0  0  1  0 NA  1  0  0 NA NA  0 NA NA  1  0  1  0
#> [18769] NA  0  1 NA  0  0  0  1  0  0  0  1 NA  1  0  0  1  1  1  0  0 NA  0  0
#> [18793]  1  0  1 NA  0  0  0  0  1  1  0  0 NA  0  1  0  0  1 NA NA  1 NA  0  0
#> [18817] NA NA  0  0  0  1 NA  1  0  0 NA  0  0  0  0  0  1  0  1  0  0  1  1  0
#> [18841]  0  0  0  1 NA  0  0  0  0  0  1  0  0  1  1  0  0  0 NA  0  0  0  0  0
#> [18865]  1  1  0  0  1  0  0  1  0  1  0  0  0  0  0 NA NA  0  0 NA  0  0  0 NA
#> [18889]  0  0  1  0  1 NA NA  1  0  0  0  1  1  0  0  0  0 NA  0 NA  0  0  1 NA
#> [18913]  1  0  0 NA  1 NA  0  1  1 NA NA NA  0  0  1  0  1  0  1  1  1  0 NA NA
#> [18937]  0  1  1  0 NA  0  0  0 NA  1  0  0  1  1  0  0  1 NA  0  0 NA NA NA  0
#> [18961]  0 NA  0 NA  1  1  1  0  1  0  1  0  0 NA  1  0  1 NA  0  0  1  1  0  0
#> [18985]  1 NA  1  0 NA  0  0  0  1  1  0  0  0 NA  0  1 NA  0  0  0 NA  0 NA  0
#> [19009]  1  0  0  0  0 NA  1  0  0 NA NA NA  1  0  1  0  1  0  0  0  1  1  0 NA
#> [19033] NA  0  1  0 NA  0  0  0  0  0  1  0 NA  0  0  0  0 NA NA NA NA NA  1  0
#> [19057]  0  0  1 NA NA  0  0 NA NA NA  1  0  1  1  0  0  1 NA  1  0  0 NA  0 NA
#> [19081]  0  0 NA NA  0  1  1  0  0  0  1  1  1 NA  1 NA  0  0  1  0  0  1  1  0
#> [19105] NA  1 NA  0  1  1 NA  0 NA  1 NA  0  1  1  1  1  0 NA  1 NA  0 NA  0 NA
#> [19129]  1  0  1  1  1  0 NA NA NA  0  0  0 NA  1  0  0  0  0  0  0  0  0  0  0
#> [19153]  1  0 NA NA  0  0  1  0  0  0  1  0 NA  0  0 NA  1  0  1  0 NA  0  0  0
#> [19177] NA  0  0  0  0  1  0  0  1  0  0  0  0  0  0  0  1 NA NA  0  0  1  0 NA
#> [19201] NA  0  0  0  0  1  0 NA NA NA  0  0  0 NA  1 NA  0  0  1  1  0 NA  0  1
#> [19225] NA  1  0  1 NA  1  1  1  0 NA  1  0  0  0  0 NA NA  1  1  0  0  0  0  1
#> [19249] NA NA  0  0 NA  1 NA  0  0  1  0  0 NA  0  1  0  0 NA  0  0  1  1 NA  0
#> [19273]  1  1  0  0  0  0  1  0  0  0  1  0 NA  0  0  1  0  0  0  0  1  0  0 NA
#> [19297]  0  0 NA  0  1  0  0  1  0 NA  0  1  0  0  0  0 NA  0  0  0  0  1  0  1
#> [19321] NA  1  0  0  0  0  0  0 NA  0  0  1  0  0 NA  0  0  0  0  1 NA  1  0  1
#> [19345]  0 NA  1  0  1  0  1 NA NA  1  0  1  1 NA NA  0  0  0  1 NA  1  1 NA  0
#> [19369]  1  0  0  0 NA NA NA NA  1  0  1  0  0 NA  1  0  0  0  0 NA  0  0  0  1
#> [19393]  0  0  1  0  0  0  0  0  1  0  0  1  1 NA  1  1  0  0  0  0  1  1  0  0
#> [19417]  0  1 NA  1  0  0  0  0  1  0 NA  1  1  1  1 NA  0  1  0  0  1  0  0  1
#> [19441]  1  0  0 NA  0 NA  0 NA NA

## Detect inflammation by AGP and CRP
detect_inflammation(crp = 2, agp = 2)
#> [1] "late convalescence"
detect_inflammation(crp = 2, agp = 2, label = FALSE)
#> [1] 3
```
