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
    flag1 <- flag_hlaz(hlaz = df[[hlaz]])
  } else {
    warning(
      "hlaz is NULL hence flagging criteria for height-for-age or length-for-age
      z-score not applied."
    )
  }

  ## Apply WHO WHLZ flagging criteria
  if(!is.null(whlz)) {
    flag2 <- flag_whlz(whlz = df[[whlz]])
  } else {
    warning(
      "hwlz is NULL hence flaggting criteria for weight-for-height or weight-
      for-length z-score not applied."
    )
  }

  ## Apply WHO WAZ flagging criteria
  if(!is.null(waz)) {
    flag4 <- flag_waz(waz = df[[waz]])
  } else {
    "waz is NULL hence flagging criteria for weight-for-age z-score not applied."
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
#' criteria to height- and/or length-for-age z-score
#'
#' @param hlaz A numeric value or vector of numeric values for
#'   *height-for-age z-score* (*haz*) and/or *length-for-age z-score* (*laz*).
#'
#' @return A numeric value or vector of values of either *0* or *1* with a
#'   value of *0* indicating that z-score value is not flagged and a value of
#'   *1* indicating that z-score value is flagged.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Check if a single height-for-age or length-for-age z-score value is
#' ## within WHO recommended limits
#' flag_hlaz(zscorer::anthro1$haz[1])
#'
#' ## Check if a vector of height-for-age and length-for-age z-score values are
#' ## within WHO recommended limits
#' flag_hlaz(zscorer::anthro1$haz)
#'
#' @export
#'
#
################################################################################

flag_hlaz <- function(hlaz = NULL) {
  flag <- vector(mode = "numeric", length = length(hlaz))
  flag <- ifelse(hlaz < -6 | hlaz > 6, flag + 1, flag)
  flag
}


################################################################################
#
#'
#' Apply World Health Organization (WHO) anthropometric z-score indices flagging
#' criteria to weight-for-height and/or weight-for-length z-score
#'
#' @param whlz A numeric value or vector of numeric values for
#'   *weight-for-height z-score* (*whz*) and/or
#'   *weight-for-length z-score* (*wlz*).
#'
#' @return A numeric value or vector of values of either *0* or *2* with a
#'   value of *0* indicating that z-score value is not flagged and a value of
#'   *2* indicating that z-score value is flagged.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Check if a single weight-for-height or weight-for-length z-score value is
#' ## within WHO recommended limits
#' flag_whlz(zscorer::anthro1$whz[1])
#'
#' ## Check if a vector of weight-for-height and weight-for-length z-score
#' ##values are within WHO recommended limits
#' flag_whlz(zscorer::anthro1$whz)
#'
#' @export
#'
#
################################################################################

flag_whlz <- function(whlz = NULL) {
  flag <- vector(mode = "numeric", length = length(whlz))
  flag <- ifelse(whlz < -5 | whlz > 5, flag + 2, flag)
  flag
}


################################################################################
#
#'
#' Apply World Health Organization (WHO) anthropometric z-score indices flagging
#' criteria to weight-for-age z-score
#'
#' @param waz A numeric value or vector of numeric values for
#'   *weight-for-age z-score* (*waz*).
#'
#' @return A numeric value or vector of values of either *0* or *4* with a
#'   value of *0* indicating that z-score value is not flagged and a value of
#'   *4* indicating that z-score value is flagged.
#'
#' @author Ernest Guevarra
#'
#' @examples
#' ## Check if a single weight-for-age z-score value is within WHO recommended
#' ## limits
#' flag_waz(zscorer::anthro1$waz[1])
#'
#' ## Check if a vector of weight-for-age z-score values are within WHO
#' ## recommended limits
#' flag_waz(zscorer::anthro1$waz)
#'
#' @export
#'
#
################################################################################

flag_waz <- function(waz = NULL) {
  flag <- vector(mode = "numeric", length = length(waz))
  flag <- ifelse(waz < -6 | waz > 5, flag + 4, flag)
  flag
}

