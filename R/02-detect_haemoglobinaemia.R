################################################################################
#
#' Determine haemoglobinaemia status for various population groups
#'
#' This set of functions identifies haemoglobinaemia in specific target groups
#' such as children under 5 years (**u5**), children 5-11 years of age
#' (**5to11**), children 12-14 years of age (**12to14**), non-pregnant women 15
#' years and above (**np_women**), pregnant women (**pregnant**), and men
#' (**men**).
#'
#' @param hb A numeric value or numeric vector containing haemoglobin values in
#'   grams per litre (g/L).
#' @param group A character value specifying the population target group to
#'   identify haemoglobinaemia from. Can be either one of **u5**, **5to11**,
#'   **11to14**, **np_women**, **pregnant**, or **men**. Default is **u5**.
#' @param label Logical. Should labels be used to classify haemoglobinaemia?
#'   If TRUE (default), status is classified as "no anaemia", "mild anaemia",
#'   "moderate anaemia", or "severe anaemia". If FALSE, simple integer codes
#'   are returned: 0 for no anaemia; 1 for mild anaemia; 2 for moderate anaemia;
#'   3 for severe anaemia.
#'
#' @returns If `label` is TRUE, a character value or character vector of
#'   haemoglobinaemia status classification (can be either "severe anaemia" or
#'   "moderate anaemia", "mild anaemia", or "no anaemia"). If `label` is FALSE,
#'   an integer value or integer vector of haemoglobinaemia status
#'   classification (0 = no anaemia; 1 = mild anaemia; 2 = moderate anaemia;
#'   3 = severe anaemia)
#'
#' @examples
#' x <- subset(mnData, age < 5)
#' detect_anaemia_u5(hb = x$hb * 10)
#' detect_anaemia_u5(hb = x$hb * 10, label = FALSE)
#' detect_anaemia(hb = x$hb * 10)
#' detect_anaemia(hb = x$hb * 10, label = FALSE)
#'
#' @author Nicholus Tint Zaw and Ernest Guevarra
#'
#' @export
#' @rdname detect_anaemia
#'
#
################################################################################

detect_anaemia_u5 <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_u5 <- cut(
      x = hb,
      breaks = c(0, 70, 100, 110, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_u5 <- as.character(anaemia_cat_u5)
  } else {
    anaemia_cat_u5 <- cut(
      x = hb,
      breaks = c(0, 70, 100, 110, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_u5 <- as.integer(as.character(anaemia_cat_u5))
  }

  ## Return
  anaemia_cat_u5
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia_5to11 <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_5to11 <- cut(
      x = hb,
      breaks = c(0, 80, 110, 115, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_5to11 <- as.character(anaemia_cat_5to11)
  } else {
    anaemia_cat_5to11 <- cut(
      x = hb,
      breaks = c(0, 80, 110, 115, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_5to11 <- as.integer(as.character(anaemia_cat_5to11))
  }

  ## Return
  anaemia_cat_5to11
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia_12to14 <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_12to14 <- cut(
      x = hb,
      breaks = c(0, 80, 110, 120, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_12to14 <- as.character(anaemia_cat_12to14)
  } else {
    anaemia_cat_12to14 <- cut(
      x = hb,
      breaks = c(0, 80, 110, 120, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_12to14 <- as.integer(as.character(anaemia_cat_12to14))
  }

  ## Return
  anaemia_cat_12to14
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia_np_women <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_np_women <- cut(
      x = hb,
      breaks = c(0, 80, 110, 120, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_np_women <- as.character(anaemia_cat_np_women)
  } else {
    anaemia_cat_np_women <- cut(
      x = hb,
      breaks = c(0, 80, 110, 120, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_np_women <- as.integer(as.character(anaemia_cat_np_women))
  }

  ## Return
  anaemia_cat_np_women
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia_pregnant <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_pregnant <- cut(
      x = hb,
      breaks = c(0, 70, 100, 110, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_pregnant <- as.character(anaemia_cat_pregnant)
  } else {
    anaemia_cat_pregnant <- cut(
      x = hb,
      breaks = c(0, 70, 100, 110, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_pregnant <- as.integer(as.character(anaemia_cat_pregnant))
  }

  ## Return
  anaemia_cat_pregnant
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia_men <- function(hb = NULL, label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (label) {
    anaemia_cat_men <- cut(
      x = hb,
      breaks = c(0, 80, 110, 130, Inf),
      labels = c("severe anaemia", "moderate anaemia",
                 "mild anaemia", "no anaemia"),
      right = FALSE
    )
    anaemia_cat_men <- as.character(anaemia_cat_men)
  } else {
    anaemia_cat_men <- cut(
      x = hb,
      breaks = c(0, 80, 110, 130, Inf),
      labels = c(3, 2, 1, 0),
      right = FALSE
    )
    anaemia_cat_men <- as.integer(as.character(anaemia_cat_men))
  }

  ## Return
  anaemia_cat_men
}


################################################################################
#
#' @export
#' @rdname detect_anaemia
#
################################################################################

detect_anaemia <- function(hb = NULL,
                           group = c("u5", "5to11", "12to14",
                                     "np_women", "pregnant", "men"),
                           label = TRUE) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  group <- match.arg(group)

  ## Determine anaemia status
  anaemia_status <- eval(
    expr = parse(
      text = paste0(
        "detect_anaemia_", group, "(hb = hb, label = label)"
      )
    )
  )

  ## Return
  anaemia_status
}
