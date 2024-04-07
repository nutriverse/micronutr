################################################################################
#
#' Determine population level iodine intake status
#'
#' Determining population level iodine intake status using mean urinary iodine
#' concentration for school age children (**general**), pregnant women
#' (**pregnant**), or lactating women with children under 2 years old
#' (**lactating**).
#'
#' @param iodine A numeric value or numeric vector of median urinary iodine
#'   concentration (microgram/L) of the population or populations of interest.
#' @param group The population group of interest. Either school age children
#'   (**general**), pregnant women (**pregnant**), or lactating women with
#'   children under 2 years old (**lactating**)
#' @param label Logical. Should labels be used to classify iodine intake
#'   status? If TRUE (default), status is classified descriptively as
#'   insufficient, adequate, excessive, or the like. If FALSE, simple integer
#'   codes are returned.
#'
#' @returns If `label` is TRUE, a character value or character vector of iodine
#'   intake status. If `label` is FALSE, an integer value or integer vector of
#'   iodine intake status.
#'
#' @examples
#' detect_iodine(iodine = 10)
#'
#' @author Nicholus Tint Zaw and Ernest Guevarra
#'
#' @export
#' @rdname detect_iodine
#'
################################################################################

detect_iodine <- function(iodine = NULL,
                          group = c("general", "pregnant", "lactating"),
                          label = TRUE){
  if (is.null(iodine)) {
    stop("Mean urinary iodine concentration required. Please try again.")
  }

  if (!is.numeric(iodine)) {
    stop("Mean urinary iodine concentration should be numeric. Please try again.")
  }

  # Get group
  group <- match.arg(group)

  ##
  iodine_status <- eval(
    parse(text = paste0("detect_iodine_", group, "(iodine, label)"))
  )

  ## Return
  iodine_status
}


################################################################################
#
#' @export
#' @rdname detect_iodine
#
################################################################################

detect_iodine_general <- function(iodine = NULL, label = TRUE) {
  if (is.null(iodine)) {
    stop("Mean urinary iodine concentration is required. Please try again.")
  }

  if (!is.numeric(iodine)) {
    stop("Mean urinary iodine concentration should be numeric. Please try again.")
  }

  if (label) {
    iodine_general <- cut(
      x = iodine,
      breaks = c(0, 20, 50, 100, 200, 300, Inf),
      labels = c("insufficient (severe)",
                 "insufficient (moderate)",
                 "insufficient (mild)",
                 "adequate",
                 "above requirement",
                 "excessive"),
      right = FALSE
    )
    iodine_general <- as.character(iodine_general)
  } else {
    iodine_general <- cut(
      x = iodine,
      breaks = c(0, 20, 50, 100, 200, 300, Inf),
      labels = FALSE,
      right = FALSE
    )
  }

  ## Return
  iodine_general
}


################################################################################
#
#' @export
#' @rdname detect_iodine
#
################################################################################

detect_iodine_pregnant <- function(iodine = NULL, label = TRUE) {
  if (is.null(iodine)) {
    stop("Mean urinary iodine concentration is required. Please try again.")
  }

  if (!is.numeric(iodine)) {
    stop("Mean urinary iodine concentration should be numeric. Please try again.")
  }

  if (label) {
    iodine_pregnant <- cut(
      x = iodine,
      breaks = c(0, 150, 250, 500, Inf),
      labels = c("insufficient", "adequate", "above requirement", "excessive"),
      right = FALSE
    )
    iodine_pregnant <- as.character(iodine_pregnant)
  } else {
    iodine_pregnant <- cut(
      x = iodine,
      breaks = c(0, 150, 250, 500, Inf),
      labels = FALSE,
      right = FALSE
    )
  }

  ## Return
  iodine_pregnant
}


################################################################################
#
#' @export
#' @rdname detect_iodine
#
################################################################################

detect_iodine_lactating <- function(iodine = NULL, label = TRUE) {
  if (is.null(iodine)) {
    stop("Mean urinary iodine concentration is required. Please try again.")
  }

  if (!is.numeric(iodine)) {
    stop("Mean urinary iodine concentration should be numeric. Please try again.")
  }

  if (label) {
    iodine_lactating <- cut(
      x = iodine,
      breaks = c(0, 100, Inf),
      labels = c("insufficient", "adequate"),
      right = FALSE
    )
    iodine_lactating <- as.character(iodine_lactating)
  } else {
    iodine_lactating <- cut(
      x = iodine,
      breaks = c(0, 100, Inf),
      labels = FALSE,
      right = FALSE
    )
  }

  ## Return
  iodine_lactating
}



