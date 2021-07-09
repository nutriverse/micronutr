################################################################################
#
#' Anaemia Diagnosis
#'
#' Perform anaemia diagnosis based on age, sex and pregnancy status
#'
#' @param df Survey dataset (as an R data.frame) with indices present
#' @param age Observation's age in year format
#' @param hb Observation's HB level in grams per litre (g/L)
#' @param sex Observation's sex category. Which is dummy variable and coded as
#'   `1` for "male" and `0` for female
#' @param pregnant Observation pregnancy status. Dummy variable with `1` for
#'   pregnant and `0` for non-pregnant
#' @param add add new generated variable (column) - `anaemia_all` which
#'   mentioned level of anameia for each observation with no-missing value in
#'   critical variables required for calculation
#' @param export exported anaemic observations for future data processing in
#'   excel file (if necessary)
#'
#' @return anaemia   A data.frame with same structure as `df` with a `anaemia_all` variable
#' @export `anemia_report.xlsx` only anameic observations were exported as excel file using file name "anemia_report.xlsx"
#'
#' @examples
#'   flag_anaemia(df = )
#'
#
################################################################################

flag_anaemia <- function(df, age = NULL, hb = NULL, sex = NULL, pregnant = NULL, add = TRUE, export = TRUE) {
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
  ##
  if(export) {
    df_anaemia <- df[!is.na(df$anaemia_all), ]
    xlsx::write.xlsx(df_anaemia, file = "anemia_report.xlsx") # error loading pakage, reltaed to Java
  }
  return(anaemia)
}



################################################################################
#
#
################################################################################


cut()
findInterval()

