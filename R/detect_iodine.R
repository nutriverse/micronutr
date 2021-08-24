################################################################################
#
#' Determining Iodine Status in Population
#'
#' Identification of population iodine intake using urinary iodine concentration
#'
#' @param x a vector which contains urinary iodine (μg/L) values.
#'
#' @param group The cut-off point for population iodine intake based on the
#'    urinary iodine value is different. Therefore, the parameter which require
#'    to identify the type of interested population for analysis need to
#'    mention in the function. The required population group for this parameter
#'    is mentioned in the below table.
#'
#'    | **Population** | **Value** |
#'    | :--- | :--- |
#'    | School-age-children (6 years or odler) |	"general" |
#'    | Pregnant Women |	"pregnant" |
#'    | Lactating Mothers and Children under 2 years old	| "lactating" |
#'
#'
#' @return iodine   the iodine intake stauts of the study (sample) population
#'
#' @examples
#'  # Create testing data
#'  x <- runif(20, min = 10, max = 350)
#'
#'  # For individual target group function;
#'   detect_iodine_general(x) # school-age-children (6 years or above)
#'   detect_iodine_pregnant(x) # pregnant
#'   detect_iodine_lactating(x) # lactating or U2 children
#'
#'
#'  # For overall population function;
#'   detect_iodine(x = x,
#'                group = c("general", "pregnant", "lactating"))
#'
#' @export
#'
#' @rdname detect_iodine
#'
################################################################################

detect_iodine <- function(x, group = c("general", "pregnant", "lactating")){

  # school-age-children (6 years old and above)
  if ("general" %in% group) {
    iodine <- detect_iodine_general(x)
  }

  # Pregnant
  if ("pregnant" %in% group) {
    iodine <- detect_iodine_pregnant(x)

  }

  # Lactating or U2 children
  if ("lactating" %in% group) {
    iodine <- detect_iodine_lactating(x)
  }

  return(iodine)
}


################################################################################
#' @export
#' @rdname detect_iodine
#'

detect_iodine_general <- function(x){
  x <- median(x, na.rm = FALSE)
  iodine_general <- cut(x,
                        breaks = c(-Inf, 20, 50, 100, 200, 300, Inf),
                        labels = c("insufficient (severe)",
                                  "insufficient (moderate)",
                                  "insufficient (mild)",
                                  "adequate", "above requirement",
                                  "Excessive"),
                        right = FALSE)
  return(iodine_general)
}

#' @export
#' @rdname detect_iodine
#'
detect_iodine_pregnant <- function(x){
  x <- median(x, na.rm = FALSE)
  iodine_pregnant <- cut(x,
                            breaks = c(-Inf, 150, 250, 500, Inf),
                            labels = c("insufficient", "adequate",
                                       "above requirement", "excessive"),
                            right = FALSE)
  return(iodine_pregnant)
}

#' @export
#' @rdname detect_iodine
#'
detect_iodine_lactating <- function(x){
  x <- median(x, na.rm = FALSE)
  iodine_lactating <- cut(x,
                             breaks = c(-Inf, 100, Inf),
                             labels = c("insufficient", "adequate"),
                             right = FALSE)
  return(iodine_lactating)
}


################################################################################
