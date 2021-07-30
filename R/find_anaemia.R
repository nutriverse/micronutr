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
#' @return anaemia   A data frame with the same structure as `df` is named
#'    `anaemia`. In this new data.frame, the new variable `anaemia_all` can be
#'    observed, containing the information which observation was affected by
#'    what type of anaemia: mild, moderate, or severe.
#'
#' # flag_anaemia(df = df,
#' #              age = age,
#' #                hb = hb
#' #                sex = sex,
#' #                pregnant = pregnant,
#' #                add = TRUE)
#'
#'
#'
#
################################################################################

flag_anaemia <- function(df, age = NULL, hb = NULL, sex = NULL, pregnant = NULL, add = TRUE) {
  ##
  anaemia_all <- vector(mode = "numeric", length = nrow(df)) # anaemia level category
  ##
  # category value: mid = 1, moderate = 2, severe = 3
  # pregnant category
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 1) & # pregnant == 1 is pregnant
                            (df$hb >= 100  & df$hb < 110 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 1) &
                            (df$hb >= 70  & df$hb < 100 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 1) &
                            (df$hb < 70 ), 3, anaemia_all)
  }
  ##
  # non-pregnant category
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 0) &
                            (df$hb >= 110  & df$hb < 120 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 0) &
                            (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(pregnant)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$pregnant) &
                            (df$age > 15 & df$pregnant == 0) &
                            (df$hb < 80 ), 3, anaemia_all)
  }
  ##
  # men category
  if(!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
                            (df$age > 15 & df$sex == 1) & # sex == 1 is male
                            (df$hb >= 110  & df$hb < 130 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
                            (df$age > 15 & df$sex == 1) &
                            (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb) & !is.na(sex)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) & !is.na(df$sex) &
                            (df$age > 15 & df$sex == 1) &
                            (df$hb < 80 ), 3, anaemia_all)
  }
  ##
  # child U5 category
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age < 5) &
                            (df$hb >= 100  & df$hb < 110 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age < 5) &
                            (df$hb >= 70  & df$hb < 100 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age < 5) &
                            (df$hb < 70 ), 3, anaemia_all)
  }
  ##
  # child 5 - 11 category
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 5 & df$age < 12) &
                            (df$hb >= 110  & df$hb < 115 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 5 & df$age < 12) &
                            (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 5 & df$age < 12) &
                            (df$hb < 80 ), 3, anaemia_all)
  }
  ##
  # child 12 - 14 category
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 12 & df$age < 15) &
                            (df$hb >= 110  & df$hb < 120 ), 1, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 12 & df$age < 15) &
                            (df$hb >= 80  & df$hb < 110 ), 2, anaemia_all)
  }
  ##
  if(!is.null(age) & !is.na(hb)) {
    anaemia_all <- ifelse(!is.na(df$age) & !is.na(df$hb) &
                            (df$age >= 12 & df$age < 15) &
                            (df$hb < 80 ), 3, anaemia_all)
  }
  ##
  if(add) {
    df$anaemia_all <- anaemia_all
    anaemia        <- df
  }

  return(anaemia)
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
#' @return anaemia   A data frame with the same structure as `df` is named
#'    `anaemia`. In this new data.frame, the new variable `anaemia_all` can be
#'    observed, containing the information which observation was affected by
#'    what type of anaemia: mild, moderate, or severe.
#'
#' #  flag_anaemia_2(df = df,
#' #                pop_group = pop_group,
#' #                hb = hb,
#' #                add = TRUE)
#'
#'
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
                              labels = c("severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia")),
                          ifelse(pop_group == 3 | pop_group == 4,
                                 cut(df$hb,
                                     breaks = c(-Inf, 80, 109, 119, Inf),
                                     labels = c("severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia")),
                                 ifelse(pop_group == 2,
                                        cut(df$hb,
                                            breaks = c(-Inf, 80, 109, 114, Inf),
                                            labels = c("severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia")),
                                        ifelse(pop_group == 6,
                                               cut(df$hb,
                                                   breaks = c(-Inf, 80, 109, 129, Inf),
                                                   labels = c("severe anaemia", "moderate anaemia", "mild anaemia", "no anaemia")),
                                               NA))))
  }

  ##
  if(add) {
    df$anaemia_all <- anaemia_all
    anaemia        <- df
  }

  return(anaemia)
}


################################################################################
# alternated function using cut
# and using separate function for different target group
################################################################################

################################################################################
#
#' Anaemia Diagnosis
#'
#' Perform anaemia diagnosis based on specific target sample groups (age, sex
#' and pregnancy status)
#'
#' This package was designed to provide the functions for identifying anaemia
#' in both individual-specific target groups and the overall population, which
#' includes all target groups' functions.
#'
#' For individual target group function;
#'
#' @param x a vector which contains haemoglobin value in grams per litre (g/L)
#'    unit format. The following table provide the specific commend based on the
#'    respective individual sample group.
#'
#'    | **Population** | **Commend** |
#'    | :--- | :--- |
#'    | Children 6-59 months of age |	name_anaemia_u5 |
#'    | Children 5-11 years of age |	name_anaemia_c5to11 |
#'    | Children 12-14 years of age	| name_anaemia_u11to14 |
#'    | Non-pregnant women (15 years and above) | name_anaemia_nonpreg_women |
#'    | Pregnant women	| name_anaemia_pregnant |
#'    | Men (15 years and above) |	name_anaemia_men |
#'
#' For overall population function;
#'
#' @param df Survey dataset (as an R data.frame) with the present of haemoglobin
#'    value variable.
#' @param hb Sample observation's haemoglobin level from the dataset. The Hb
#'    values should record in the grams per liter (g/L) units.
#' @param group This is the vector of the specific population target group you
#'    want to include in identifying anaemia in the overall population function.
#'    In this `name_anameia` package, the function can determine the anaemia
#'    status of the following groups, and you need to mention them in vector
#'    format depend on which sample groups of population include in your
#'    dataset. Please use the following short-form of each target name in
#'    construction your vector. If not, the commend will not recognize
#'    the function.
#'
#'    | **Population** | **Function recognized Short-form** |
#'    | :--- | :--- |
#'    | Children 6-59 months of age |	"u5" |
#'    | Children 5-11 years of age |	"c5to11" |
#'    | Children 12-14 years of age	| "u11to14" |
#'    | Non-pregnant women (15 years and above) | "nonpreg_women" |
#'    | Pregnant women	| "pregnant" |
#'    | Men (15 years and above) |	"men" |
#'
#'    if the dataset did not include this variable yet, please create one before
#'    using this function. Sample r code for generating this variable is
#'    mentioned below using the sample dataset `hbData`.
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
#' @return anaemia   A data frame with the same structure as `df` is named
#'    `anaemia`. In this new data.frame, the new variable `anaemia_all` can be
#'    observed, containing the information which observation was affected by
#'    what type of anaemia: mild, moderate, or severe.
#'
#' @examples
#'  # Create testing data
#'  x <- runif(20, min = 60, max = 130)
#'
#'  hb <- runif(50, min = 60, max = 130)
#'  gender <- rep(c("male", "female"), each = 25)
#'
#'  df <- data.frame(gender, hb)
#'
#'  # For individual target group function;
#'   name_anaemia_u5(x) # U5 Children
#'   name_anaemia_c5to11(x) # Children 5 - 11 years
#'   name_anaemia_c12to14(x) # Children 12 - 14 years
#'   name_anaemia_nonpreg_women(x) # Non-pregnant Women
#'   name_anaemia_pregnant(x) # Pregnant Women
#'   name_anaemia_men(x) # Men
#'
#'
#'  # For overall population function;
#'   name_anaemia(df = df,
#'                hb = hb,
#'                group = c("u5", "c5to11", "c12to14", "nonpreg_women",
#'                        "pregnant", "men"),
#'                add = TRUE)
#'
#' @export
#'
#' @rdname name_anaemia
#'
#'
#
################################################################################

name_anaemia <- function(df, group = c("u5", "c5to11", "c12to14",
                                       "nonpreg_women", "pregnant", "men"),
                         hb = NULL, add = TRUE){
  group <- match.arg(group)

  # U5 children
  if (group == "u5") {
    name_anaemia_u5(df$hb)

    if(add) {
        df$anaemia_cat_u5 <- name_anaemia_u5(df$hb)
        anaemia        <- df
      }
  }

  # Children 5 - 11 years
  else if (group == "c5to11") {
    name_anaemia_c5to11(df$hb)

    if(add) {
      df$anaemia_cat_c5to11 <- name_anaemia_c5to11(df$hb)
      anaemia        <- df
    }
  }

  # Children 12 - 14 years
  else if (group == "c12to14") {
    name_anaemia_c12to14(df$hb)

    if(add) {
      df$anaemia_cat_c12to14 <- name_anaemia_c12to14(df$hb)
      anaemia        <- df
    }
  }

  # Non-pregnant Women
  else if (group == "nonpreg_women") {
    name_anaemia_nonpreg_women(df$hb)

    if(add) {
      df$anaemia_cat_nonpreg_women <- name_anaemia_nonpreg_women(df$hb)
      anaemia        <- df
    }
  }

  # Pregnant Women
  else if (group == "pregnant") {
    name_anaemia_pregnant(df$hb)

    if(add) {
      df$anaemia_cat_pregnant <- name_anaemia_pregnant(df$hb)
      anaemia        <- df
    }
  }

  # Men
  else if (group == "men") {
    name_anaemia_men(df$hb)

    if(add) {
      df$anaemia_cat_men <- name_anaemia_men(df$hb)
      anaemia        <- df
    }
  }

  return(anaemia)
}


################################################################################
#' @export
#' @rdname name_anaemia
#'

name_anaemia_u5 <- function(x){
  anaemia_cat_u5 <- cut(x,
                        breaks = c(-Inf, 70, 100, 110, Inf),
                        labels = c("severe anaemia", "moderate anaemia",
                                   "mild anaemia", "no anaemia"),
                        right = FALSE)
  return(anaemia_cat_u5)
}

#' @export
#' @rdname name_anaemia
#'
name_anaemia_c5to11 <- function(x){
  anaemia_cat_c5to11 <- cut(x,
                            breaks = c(-Inf, 80, 110, 115, Inf),
                            labels = c("severe anaemia", "moderate anaemia",
                                       "mild anaemia", "no anaemia"),
                            right = FALSE)
  return(anaemia_cat_c5to11)
}

#' @export
#' @rdname name_anaemia
#'
name_anaemia_c12to14 <- function(x){
  anaemia_cat_c12to14 <- cut(x,
                             breaks = c(-Inf, 80, 110, 120, Inf),
                             labels = c("severe anaemia", "moderate anaemia",
                                        "mild anaemia", "no anaemia"),
                             right = FALSE)
  return(anaemia_cat_c12to14)
}

#' @export
#' @rdname name_anaemia
#'
name_anaemia_nonpreg_women <- function(x){
  anaemia_cat_nonpreg_women <- cut(x,
                           breaks = c(-Inf, 80, 110, 120, Inf),
                           labels = c("severe anaemia", "moderate anaemia",
                                      "mild anaemia", "no anaemia"),
                           right = FALSE)
  return(anaemia_cat_nonpreg_women)
}

#' @export
#' @rdname name_anaemia
#'
name_anaemia_pregnant <- function(x){
  anaemia_cat_pregnant <- cut(x,
                          breaks = c(-Inf, 70, 100, 110, Inf),
                          labels = c("severe anaemia", "moderate anaemia",
                                     "mild anaemia", "no anaemia"),
                          right = FALSE)
  return(anaemia_cat_pregnant)
}


#' @export
#' @rdname name_anaemia
#'
name_anaemia_men <- function(x){
  anaemia_cat_men <- cut(x,
                         breaks = c(-Inf, 80, 110, 130, Inf),
                         labels = c("severe anaemia", "moderate anaemia",
                                    "mild anaemia", "no anaemia"),
                         right = FALSE)
  return(anaemia_cat_men)
}

################################################################################
