################################################################################
#
#'
#' Apply World Health Organization (WHO) anthropometric z-score indices flagging
#' criteria
#'
#' Flagging is a process of checking whether values of anthropometric indices
#' are outside a given range and recording the result in one or more new
#' variables. The result may be a set of logical (i.e. 1/0 or true/false) flag
#' variables (i.e. one flag variable per anthropometric index) or a single
#' variable holding a code number that classifies the nature of the detected
#' problem(s).
#'
#' The WHO flagging criteria are simple biologically plausible ranges. If, for
#' example, a value for weight-for-height z-score is below -5 or above +5 then
#' the record is flagged to indicate a likely problem with weight-for-height
#' z-score. This will usually be caused by an erroneous weight or height value
#' being recorded.
#'
#' @param df A data.frame containing anthropometric z-score indices for
#'   *height-for-age* or *length-for-age*, *weight-for-age*, and/or
#'   *weight-for-height* or *weight-for-length*.
#' @param hlaz A character value indicating the variable name in `df` for the
#'   *height-for-age* or *length-for-age* z-score.
#' @param waz A character value indicating the variable name in `df` for the
#'   *weight-for-age* z-score.
#' @param whlz A character value indicating the variable name in `df` for the
#'   *weight-for-height* or *weight-for-length* z-score.
#' @param verbose Logical. Should an additional flag description be returned?
#'   Default is TRUE.
#' @param add Logical. Should flag values be added to `df`. Default is TRUE.
#'
#' @return Returns `df` with additional column named `flag` containing coded
#'   values indicating problematic measurements and a column named
#'   `flag_description` containing text describing which anthropometric
#'   measurement/s are likely problematic. If `add` FALSE and `verbose` FALSE,
#'   returns a vector of `flag` coded values indicating problematic
#'   measurements. if `add` FALSE and `verbose` TRUE, returns a data.frame with
#'   a column named `flag` containing coded values indicating problematic
#'   measurements and a column named `flag_description` containing text
#'   describing which anthropometric measurement/s are likely problematic.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' flag_who(df = zscorer::anthro1, hlaz = "haz", waz = "waz", whlz = "whz")
#'
#' @rdname flag_who
#' @export
#'
#
################################################################################

flag_who <- function(df, hlaz = NULL, waz = NULL, whlz = NULL,
                     verbose = TRUE, add = TRUE) {
  ## Apply WHO HLAZ flagging criteria
  if(!is.null(hlaz)) {
    flag1 <- flag_zscore(x = df[[hlaz]], z = "hlaz")
  } else {
    warning(
      paste(
        strwrap(
          x = "hlaz is NULL hence flagging criteria for height-for-age or
               length-for-age z-score not applied.",
          width = 80
        ),
        collapse = "\n"
      )
    )
  }

  ## Apply WHO WHLZ flagging criteria
  if(!is.null(whlz)) {
    flag2 <- flag_zscore(x = df[[whlz]], z = "whlz")
  } else {
    warning(
      paste(
        strwrap(
          x = "hwlz is NULL hence flaggting criteria for weight-for-height or
               weight-for-length z-score not applied.",
          width = 80
        ),
        collapse = "\n"
      )
    )
  }

  ## Apply WHO WAZ flagging criteria
  if(!is.null(waz)) {
    flag4 <- flag_zscore(x = df[[waz]], z = "waz")
  } else {
    paste(
      strwrap(
        x = "waz is NULL hence flagging criteria for weight-for-age z-score not
             applied.",
        width = 80
      ),
      collapse = "\n"
    )
  }

  ## Sum flag codes and create flag_description
  flag <- flag1 + flag2 + flag4
  flag_description <- vector(mode = "character", length = length(flag))
  flag_description[flag == 0] <- "No flagged measurements"
  flag_description[flag == 1] <- "Check height and age measurements"
  flag_description[flag == 2] <- "Check weight and height measurements"
  flag_description[flag == 3] <- "Check height measurement"
  flag_description[flag == 4] <- "Check weight and age measurements"
  flag_description[flag == 5] <- "Check age measurement"
  flag_description[flag == 6] <- "Check weight measurement"
  flag_description[flag == 7] <- "Check age, height and weight measurements"

  if (verbose) {
    flag <- data.frame(flag, flag_description)
  }

  ## Check if add is TRUE
  if(add) {
    flag <- data.frame(df, flag)
  }

  ## Return flag
  flag
}


################################################################################
#
#'
#' Apply World Health Organization (WHO) anthropometric z-score indices flagging
#' criteria to various z-score values
#'
#' @param x A numeric value or vector of numeric values for
#'   *height-for-age z-score* (*haz*) and/or *length-for-age z-score* (*laz*),
#'   *weight-for-height z-score* (*whz*) and/or *weight-for-length z-score*
#'   (*wlz*), or *weight-for-age z-score* (*waz*).
#'
#' @param z A character value for type of z-score value provided. Can be one of
#'   either *hlaz* for *height-for-age* or *length-for-age* *z-score*, *whlz*
#'   for *weight-for-height* or *weight-for-length* *z-score*, or *waz* for
#'   *weight-for-age z-score*. If no value selected, will default to *hlaz*.
#'
#' @return A numeric value or vector of values indicating whether a z-score
#'   value/s is/are flagged. For *height-for-age* or *length-for-age* *z-score*,
#'   this will be values of either *0* or *1* with a value of *0* indicating
#'   that z-score value is not flagged and a value of *1* indicating that
#'   z-score value is flagged. For *weight-for-height* or *weight-for-length*
#'   *z-score*, this will be values of either *0* or *2* with a
#'   value of *0* indicating that z-score value is not flagged and a value of
#'   *2* indicating that z-score value is flagged. For *weight-for-age z-score*,
#'   this will be values of either *0* or *4* with a value of *0* indicating
#'   that z-score value is not flagged and a value of *4* indicating that
#'   z-score value is flagged.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Check if a single height-for-age or length-for-age z-score value is
#' ## within WHO recommended limits
#' flag_zscore(x = zscorer::anthro1$haz[1], z = "hlaz")
#'
#' ## Check if a vector of height-for-age and length-for-age z-score values are
#' ## within WHO recommended limits
#' flag_zscore(x = zscorer::anthro1$haz, z = "hlaz")
#'
#' ## Check if a single weight-for-height or weight-for-length z-score value is
#' ## within WHO recommended limits
#' flag_zscore(x = zscorer::anthro1$whz[1], z = "whlz")
#'
#' ## Check if a vector of weight-for-height and weight-for-length z-score
#' ## values are within WHO recommended limits
#' flag_zscore(x = zscorer::anthro1$whz, z = "whlz")
#'
#' ## Check if a single weight-for-age z-score value is within WHO recommended
#' ## limits
#' flag_zscore(x = zscorer::anthro1$waz[1], z = "waz")
#'
#' ## Check if a vector of weight-for-age z-score values are within WHO
#' ## recommended limits
#' flag_zscore(x = zscorer::anthro1$waz, z = "waz")
#'
#' @rdname flag_who
#' @export
#'
#
################################################################################

flag_zscore <- function(x = NULL, z = c("hlaz", "whlz", "waz")) {
  ## Get value for z
  z <- match.arg(z)

  ## Create empty flag vector
  flag <- vector(mode = "numeric", length = length(x))

  ## if z == "hlaz"
  if (z == "hlaz") {
    flag <- ifelse(x < -6 | x > 6, flag + 1,
      ifelse(is.na(x), 0, flag)
    )
  }

  ## if z == "whlz"
  if (z == "whlz") {
    flag <- ifelse(x < -5 | x > 5, flag + 2,
      ifelse(is.na(x), 0, flag)
    )
  }

  ## if z == "waz"
  if (z == "waz") {
    flag <- ifelse(x < -6 | x > 5, flag + 4,
      ifelse(is.na(x), 0, flag)
    )
  }

  ## Return flag
  flag
}


