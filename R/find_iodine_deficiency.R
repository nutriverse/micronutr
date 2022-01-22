################################################################################
#
#' Determining Iodine Status in Population
#'
#' Identification of population iodine intake using urinary iodine concentration
#'
#' @param x Either the single median value of the urinary iodine (μg/L) of the
#'   population of interest or the vector of urinary iodine (μg/L) values of the
#'   population of interest.
#'
#' @param group The population group of interest as described below:
#'
#'   | **Population** | **Short-form** |
#'   | :--- | :--- |
#'   | School-age-children (6 years or odler) |	"general" |
#'   | Pregnant Women |	"pregnant" |
#'   | Lactating Mothers and Children under 2 years old	| "lactating" |
#'
#'   This should be specified using the short-form of the population group of
#'   interest. Default to *"general"*.
#'
#' @return A character vector indicating the iodine intake status of the
#'   population of interest.
#'
#' @examples
#'  # Create testing data
#'  x <- runif(20, min = 10, max = 350)
#'
#'  # For individual target group function;
#'   detect_iodine_general(x)   # school-age-children (6 years or above)
#'   detect_iodine_pregnant(x)  # pregnant
#'   detect_iodine_lactating(x) # lactating or U2 children
#'
#'
#'  # For overall population function;
#'   detect_iodine(x = x, group = "general")
#'
#'   detect_iodine(x = x, group = "pregnant")
#'
#'   detect_iodine(x = x, group = "lactating")
#'
#'  # test with sample dataset
#'  dta <- subset(iodine, group == "Child")
#'  detect_iodine(x = dta$iodine, group = "general")
#'
#' @export
#' @rdname detect_iodine
#'
################################################################################

detect_iodine <- function(x, group = c("general", "pregnant", "lactating")){
  # Get group
  group <- match.arg(group)

  ##
  iodine_status <- eval(
    parse(text = paste0("detect_iodine_", group, "(x)"))
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

detect_iodine_general <- function(x) {
  x <- stats::median(x, na.rm = FALSE)
  iodine_general <- cut(x,
                        breaks = c(-Inf, 20, 50, 100, 200, 300, Inf),
                        labels = c("insufficient (severe)",
                                   "insufficient (moderate)",
                                   "insufficient (mild)",
                                   "adequate",
                                   "above requirement",
                                   "excessive"),
                        right = FALSE)
  iodine_general <- as.character(iodine_general)

  ## Return
  iodine_general
}


################################################################################
#
#' @export
#' @rdname detect_iodine
#
################################################################################

detect_iodine_pregnant <- function(x) {
  x <- stats::median(x, na.rm = FALSE)
  iodine_pregnant <- cut(x,
                         breaks = c(-Inf, 150, 250, 500, Inf),
                         labels = c("insufficient", "adequate",
                                    "above requirement", "excessive"),
                         right = FALSE)
  iodine_pregnant <- as.character(iodine_pregnant)

  ## Return
  iodine_pregnant
}


################################################################################
#
#' @export
#' @rdname detect_iodine
#
################################################################################

detect_iodine_lactating <- function(x) {
  x <- stats::median(x, na.rm = FALSE)
  iodine_lactating <- cut(x,
                          breaks = c(-Inf, 100, Inf),
                          labels = c("insufficient", "adequate"),
                          right = FALSE)
  iodine_lactating <- as.character(iodine_lactating)

  ## Return
  iodine_lactating
}



