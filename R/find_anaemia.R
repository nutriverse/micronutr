################################################################################
#
#' Determine anaemia status for various population groups
#'
#' This set of functions identifies anaemia in both individual-specific target
#' groups and the overall population, which includes all target groups'
#' functions.
#'
#' @param x A numeric vector containing haemoglobin values in grams per litre
#'   (g/L) unit format for a specific population group. The following table
#'   provides the specific command based on the respective population group.
#'
#'   | **Population** | **Command** |
#'   | :--- | :--- |
#'   | Children 6-59 months of age | find_anaemia_u5 |
#'   | Children 5-11 years of age | find_anaemia_5to11 |
#'   | Children 12-14 years of age | find_anaemia_11to14 |
#'   | Non-pregnant women - 15 years and above | find_anaemia_np_women |
#'   | Pregnant women	| find_anaemia_pregnant |
#'   | Men - 15 years and above |	find_anaemia_men |
#'
#' @param df A data.frame of survey dataset with information on haemoglobin
#'   value and information on population group to which the sample comes from.
#' @param hb A characater value specifying the variable name in `df` containing
#'   the sample observation's haemoglobin level. Note that the haemoglobin
#'   values should be recorded in grams per liter (g/L) units.
#'   If NULL (default), will attempt to find variable in `df` that is
#'   likely to hold the haemoglobin levels information by searching for most
#'   common variable names used for haemoglobin. If none can be found, will
#'   issue an error that appropriate haemoglobin values cannot be found.
#' @param group A character value specifying the population target group to
#'   identify anaemia from. The function can determine the anaemia status for
#'   the following groups:
#'
#'   | **Population** | **Short-form** |
#'   | :--- | :--- |
#'   | Children 6-59 months of age |	"u5" |
#'   | Children 5-11 years of age |	"5to11" |
#'   | Children 12-14 years of age	| "11to14" |
#'   | Non-pregnant women - 15 years and above | "np_women" |
#'   | Pregnant women	| "pregnant" |
#'   | Men - 15 years and above |	"men" |
#'
#'   The short-form is used in the function to indicate which population group
#'   to find anaemia from.
#'
#' @param add Logical. Should anaemia classification results be added to `df`?
#'   Default to TRUE which will add a variable named `anaemia_status` to `df`.
#'
#' @return If `add` is TRUE, a data frame with the same structure as `df` and
#'   with a new variable `anaemia_status` otherise a vector of class factor.
#'   The new variable or the resulting vector provides anaemia categories
#'   to each observation: *no anaemia*, *mild anaemia*, *moderate anaemia*, and
#'   *severe anaemia*. The following table explains the cut-off points to
#'   classify each observation accordingly:
#'
#'   | **Population** | **Mild** |	**Moderate** | **Severe** |
#'   | :--- | :--- | :--- | :--- |
#'   | Children 6-59 months of age |	100 - 109 |	70 - 99 |	< 70 |
#'   | Children 5-11 years of age |	110 - 114	| 80 - 109 |	< 80 |
#'   | Children 12-14 years of age	| 110 - 119	| 80 - 109 |	< 80 |
#'   | Non-pregnant women |  |  |  |
#'   | 15 years and above | 110 - 119	| 80 - 109 | < 80 |
#'   | Pregnant women	| 100 - 109	| 70 - 99	| < 70 |
#'   |Men |  |  |  |
#'   | 15 years and above |	110 - 129 |	80 - 109 |	< 80 |
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
#'   find_anaemia_u5(x)          # U5 Children
#'   find_anaemia_5to11(x)       # Children 5 - 11 years
#'   find_anaemia_12to14(x)      # Children 12 - 14 years
#'   find_anaemia_np_women(x)    # Non-pregnant Women
#'   find_anaemia_pregnant(x)    # Pregnant Women
#'   find_anaemia_men(x)         # Men
#'
#'
#'  # For overall population function;
#'   find_anaemia(df = df,
#'                hb = "hb",
#'                add = TRUE)
#'
#' @author Nicholus Tint Zaw
#'
#' @export
#' @rdname find_anaemia
#'
#
################################################################################

find_anaemia <- function(df,
                         group = c("u5", "5to11", "12to14",
                                   "np_women", "pregnant", "men"),
                         hb = NULL,
                         add = TRUE) {
  ## Determine haemoglobin variable if hb = NULL
  if (is.null(hb)) {
    hb <- grep(
      pattern = "hb|HB|Hb|hem|
                 Hemoglobin|HEMOGLOBIN|
                 haemoglobin|Haemoglobin|HAEMOGLOBIN",
      x = names(df),
      value = TRUE)

    if (length(hb) == 0) {
      stop("Variable for haemoglobin values not found. Please specify hb.")
    }

    if (length(hb) > 1) {
      hb <- hb[1]
      warning("More than one variable for haemoglobin values found. Will use
              the first one found")
    }
  } else {
    hb <- grep(pattern = hb, x = names(df), fixed = TRUE, value = TRUE)
    if (length(hb) == 0) {
      stop("Variable name provided for haemoglobin values cannot be found.
           Please check and try again.")
    }
  }

  ## Check that hb are numeric values
  if (class(df[[hb]]) != "numeric") {
    stop("Haemoglobin values must be numeric.")
  }

  ## Get group
  group <- match.arg(group)

  ## Determine anaemia status
  anaemia_status <- eval(
    expr = parse(text = paste0("find_anaemia_", group, "(df[[hb]])"))
  )

  ## Add anaemia to df?
  if (add) {
    anaemia <- data.frame(df, anaemia_status)
  }

  ## Return
  anaemia
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_u5 <- function(x) {
  anaemia_cat_u5 <- cut(
    x, breaks = c(-Inf, 70, 100, 110, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_u5
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_5to11 <- function(x) {
  anaemia_cat_5to11 <- cut(
    x, breaks = c(-Inf, 80, 110, 115, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_5to11
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_12to14 <- function(x) {
  anaemia_cat_12to14 <- cut(
    x, breaks = c(-Inf, 80, 110, 120, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_12to14
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_np_women <- function(x) {
  anaemia_cat_np_women <- cut(
    x, breaks = c(-Inf, 80, 110, 120, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_np_women
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_pregnant <- function(x) {
  anaemia_cat_pregnant <- cut(
    x, breaks = c(-Inf, 70, 100, 110, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_pregnant
}


################################################################################
#
#' @export
#' @rdname find_anaemia
#
################################################################################

find_anaemia_men <- function(x) {
  anaemia_cat_men <- cut(
    x, breaks = c(-Inf, 80, 110, 130, Inf),
    labels = c("severe anaemia", "moderate anaemia",
               "mild anaemia", "no anaemia"),
    right = FALSE
  )

  ## Return
  anaemia_cat_men
}


