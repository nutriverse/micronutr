################################################################################
#
#' Anaemia Diagnosis
#'
#' Perform anaemia diagnosis based on age, sex and pregnancy status
#'
#' @param df Survey dataset (as an R data.frame) with the following variables
#'    present; sex, age, haemoglobin value, and pregnancy status. If the study
#'    sample population did not contain the pregnancy population, the pregnancy
#'    status (dummy variable yes/no) might not be required for this function.
#'    But, the other variables are compulsory for function processing. If the
#'    dataset did not contain those variables, please perform data processing
#'    to have those variables in the dataset.
#' @param age Sample observation's age from the dataset. This variable should
#'    be recorded in the year format. If your dataset has only an age variable
#'    with month, please converts it into year format before using this function.
#' @param hb Sample observation's haemoglobin level from the dataset. The Hb
#'    values should record in the grams per liter (g/L) units.
#' @param sex Sample observation's sex identifies from the dataset. This
#'    variable should be coded in the dummy variable as `1` for "male" and `0`
#'    for "female". If your dataset's respondent sex variable was not recorded
#'    this way, please make necessary data recoding before using this function.
#' @param pregnant Sample observation's pregnancy status from the dataset. This
#'    variable should be coded as a dummy variable: `1` for pregnant and `0`
#'    for non-pregnant. Please perform the necessary data processing for
#'    recoding if your dataset observation's pregnancy status was not in dummy
#'    variable format.
#' @param add This parameter's default option is TRUE and will add new
#'    generated variables `anaemia_all` to your existing dataset applied in
#'    this function. This newly developed categorical variable comprises three
#'    types of flag categories resulting from data checking results; no anaemia,
#'     mild anaemia, moderate anaemia, and severe anaemia. The following table
#'     explains the cut-off points applied in this diagnostic function.
#'
#'    | **Population** | **Mild** |	**Moderate** | **Severe** |
#'    | :--- | :--- | :--- | :--- |
#'    | Children 6-59 months of age |	100 - 109 |	70 - 99 |	< 70 |
#'    | Children 5-11 years of age |	110 - 114	| 80 - 109 |	< 80 |
#'    | Children 12-14 years of age	| 110 - 119	| 80 - 109 |	< 80 |
#'    | Non-pregnant women |  |  |  |
#'    | (15 years and above) | 110 - 119	| 80 - 109 | < 80 |
#'    |Pregnant women	| 100 - 109	| 70 - 99	| < 70 |
#'    |Men |  |  |  |
#'    |(15 years and above) |	110 - 129 |	80 - 109 |	< 80 |
#'
#'
#' @return A data frame with the same structure as `df`. The new variable
#'   `anaemia_all` can be observed, containing the information the type of
#'   anaemia present: mild, moderate, or severe.
#'
#
################################################################################

flag_anaemia <- function(df,
                         age = NULL, hb = NULL, sex = NULL, pregnant = NULL,
                         add = TRUE) {
  ## Create concatenating vector
  anaemia_all <- vector(mode = "numeric", length = nrow(df))

  # category value: mild = 1, moderate = 2, severe = 3
  # pregnant category
  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 1) &
        (df$hb >= 100  & df$hb < 110 ), 1, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 1) &
        (df$hb >= 70  & df$hb < 100 ), 2, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 1) &
        (df$hb < 70 ), 3, anaemia_all
    )
  }

  # non-pregnant category
  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 0) &
        (df$hb >= 110  & df$hb < 120 ), 1, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 0) &
        (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
        (df$age > 15 & df$pregnant == 0) &
        (df$hb < 80 ), 3, anaemia_all
    )
  }

  # men category
  if (!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
        (df$age > 15 & df$sex == 1) &
        (df$hb >= 110  & df$hb < 130 ), 1, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
        (df$age > 15 & df$sex == 1) &
        (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
        (df$age > 15 & df$sex == 1) &
        (df$hb < 80 ), 3, anaemia_all
    )
  }

  # child U5 category
  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) &
        (df$age < 5) & (df$hb >= 100  & df$hb < 110 ), 1, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) &
        (df$age < 5) & (df$hb >= 70  & df$hb < 100 ), 2, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age < 5) &
        (df$hb < 70 ), 3, anaemia_all
    )
  }

  # child 5 - 11 category
  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 5 & df$age < 12) &
        (df$hb >= 110  & df$hb < 115 ), 1, anaemia_all)
  }

  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 5 & df$age < 12) &
        (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 5 & df$age < 12) &
        (df$hb < 80 ), 3, anaemia_all
    )
  }

  # child 12 - 14 category
  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 12 & df$age < 15) &
        (df$hb >= 110  & df$hb < 120 ), 1, anaemia_all
    )
  }

  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 12 & df$age < 15) &
        (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all
    )
  }


  if (!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(
      !is.na(df$age) & !is.na(df$hb) & (df$age >= 12 & df$age < 15) &
        (df$hb < 80 ), 3, anaemia_all
    )
  }

  if (add) {
    df$anaemia_all <- anaemia_all
    anaemia <- df
  }

  ## Return
  anaemia
}

################################################################################
# alternated function using cut
################################################################################

################################################################################
#
#' Anaemia Diagnosis
#'
#' Perform anaemia diagnosis based on age, sex and pregnancy status
#'
#' @param df Survey dataset (as an R data.frame) with the following variables
#'    present; sex, age, haemoglobin value, and pregnancy status. If the study
#'    sample population did not contain the pregnancy population, the pregnancy
#'    status (dummy variable yes/no) might not be required for this function.
#'    But, the other variables are compulsory for function processing. If the
#'    dataset did not contain those variables, please perform data processing
#'    to have those variables in the dataset.
#' @param pop_group This variable contains information about the characteristic
#'    of observation required to identify the anaemia status. In anaemia diagnosis,
#'    Hb thresholds are different among different populations, such as age group,
#'    gender, and special condition (like pregnancy). Therefore, one variable
#'    can indicate the type of sample population characteristic requirement for
#'    anaemia diagnosis. The required population group for this variable is
#'    mentioned in the below table.
#'
#'    | **Population** | **Valye** |
#'    | :--- | :--- |
#'    | Children 6-59 months of age |	1 |
#'    | Children 5-11 years of age |	2 |
#'    | Children 12-14 years of age	| 3 |
#'    | Non-pregnant women (15 years and above) | 4 |
#'    | Pregnant women	| 5 |
#'    | Men (15 years and above) |	6 |
#'
#'    if the dataset did not include this variable yet, please create one before
#'    using this function. Sample r code for generating this variable is
#'    mentioned below using the sample dataset `hbData`.
#'
#' df$pop_group <- with(df,ifelse(
#'  m.age >= 6 & m.age < 60, 1, ifelse(
#'  ch.age >= 5 & ch.age < 12, 2, ifelse(
#'  ch.age >= 12 & ch.age < 15, 3, ifelse(
#'  ch.age >= 15 & !is.na(ch.age) & sex == 2, 4, ifelse(
#'  group == "Pregnant Not Principal Carer" |
#'    group == "Pregnant and lactating Principal Carer" |
#'    group == "Pregnant Principal carer", 5, ifelse(
#'  ch.age >= 15 & !is.na(ch.age) & sex == 1, 6, NA)))))))
#'
#'
#' @param hb Sample observation's haemoglobin level from the dataset. The Hb
#'    values should record in the grams per liter (g/L) units.
#' @param add This parameter's default option is TRUE and will add new
#'    generated variables `anaemia_all` to your existing dataset applied in
#'    this function. This newly developed categorical variable comprises three
#'    types of flag categories resulting from data checking results; no anaemia,
#'     mild anaemia, moderate anaemia, and severe anaemia. The following table
#'     explains the cut-off points applied in this diagnostic function.
#'
#'    | **Population** | **Mild** |	**Moderate** | **Severe** |
#'    | :--- | :--- | :--- | :--- |
#'    | Children 6-59 months of age |	100 - 109 |	70 - 99 |	< 70 |
#'    | Children 5-11 years of age |	110 - 114	| 80 - 109 |	< 80 |
#'    | Children 12-14 years of age	| 110 - 119	| 80 - 109 |	< 80 |
#'    | Non-pregnant women |  |  |  |
#'    | (15 years and above) | 110 - 119	| 80 - 109 | < 80 |
#'    |Pregnant women	| 100 - 109	| 70 - 99	| < 70 |
#'    |Men |  |  |  |
#'    |(15 years and above) |	110 - 129 |	80 - 109 |	< 80 |
#'
#'
#' @return A data frame with the same structure as `df` is named
#'    `anaemia`. In this new data.frame, the new variable `anaemia_all` can be
#'    observed, containing the information which observation was affected by
#'    what type of anaemia: mild, moderate, or severe.
#'
#
################################################################################


flag_anaemia_2 <- function(df, pop_group = NULL, hb = NULL, add = TRUE) {
  ##
  anaemia_all <- vector(mode = "numeric", length = nrow(df)) # anaemia level category
  ##
  # All group
  if(!is.na(pop_group) & !is.na(hb)){
    anaemia_all <- ifelse(pop_group == 1 | pop_group == 5,
                          cut(df$hb,
                              breaks = c(-Inf, 70, 99, 109, Inf),
                              labels = c("severe anaemia",
                                         "moderate anaemia",
                                         "mild anaemia",
                                         "no anaemia")),
                          ifelse(pop_group == 3 | pop_group == 4,
                                 cut(df$hb,
                                     breaks = c(-Inf, 80, 109, 119, Inf),
                                     labels = c("severe anaemia",
                                                "moderate anaemia",
                                                "mild anaemia",
                                                "no anaemia")),
                                 ifelse(pop_group == 2,
                                        cut(df$hb,
                                            breaks = c(-Inf, 80, 109, 114, Inf),
                                            labels = c("severe anaemia",
                                                       "moderate anaemia",
                                                       "mild anaemia",
                                                       "no anaemia")),
                                        ifelse(pop_group == 6,
                                               cut(df$hb,
                                                   breaks = c(-Inf, 80, 109, 129, Inf),
                                                   labels = c("severe anaemia",
                                                              "moderate anaemia",
                                                              "mild anaemia",
                                                              "no anaemia")),
                                               NA)
                                 )
                          )
    )
  }

  ##
  if(add) {
    df$anaemia_all <- anaemia_all
    anaemia        <- df
  }

  return(anaemia)
}

