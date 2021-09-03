################################################################################
#
#' Determining Iodine Status in Population
#'
#' Identification of population iodine intake using urinary iodine concentration
#'
#' @param x This parameter accepts either the single median value of the
#'    interested population urinary iodine (μg/L) values or the vector which
#'    contains urinary iodine (μg/L) values of the interest population.
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
#'    Please note that the group parameter only accepts a single value of the
#'    specific group's name, not the combination of more than one groups' name
#'    in vector form. (for example, group == "general")
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
#'                group = "general")
#'
#'   detect_iodine(x = x,
#'                group = "pregnant")
#'
#'   detect_iodine(x = x,
#'                group = "lactating")
#'
#'  # test with sample dataset
#'  dta <- subset(iodine_sample, group == "Child")
#'  detect_iodine(x = dta$iodine,
#'                group = "general")
#'
#'
#' @export
#'
#' @rdname detect_iodine
#'
################################################################################

detect_iodine <- function(x, group = NULL){

  # school-age-children (6 years old and above)
  if (group == "general") {
    iodine <- detect_iodine_general(x)
  }

  # Pregnant
  if (group == "pregnant") {
    iodine <- detect_iodine_pregnant(x)

  }

  # Lactating or U2 children
  if (group == "lactating") {
    iodine <- detect_iodine_lactating(x)
  }

  return(iodine)
}


################################################################################
#' @export
#' @rdname detect_iodine
#'

detect_iodine_general <- function(x){
  x <- stats::median(x, na.rm = FALSE)
  iodine_general <- cut(x,
                        breaks = c(-Inf, 20, 50, 100, 200, 300, Inf),
                        labels = c("insufficient (severe)",
                                  "insufficient (moderate)",
                                  "insufficient (mild)",
                                  "adequate", "above requirement",
                                  "Excessive"),
                        right = FALSE)
  iodine_general <- as.character(iodine_general)
  return(iodine_general)
}

#' @export
#' @rdname detect_iodine
#'
detect_iodine_pregnant <- function(x){
  x <- stats::median(x, na.rm = FALSE)
  iodine_pregnant <- cut(x,
                            breaks = c(-Inf, 150, 250, 500, Inf),
                            labels = c("insufficient", "adequate",
                                       "above requirement", "excessive"),
                            right = FALSE)
  iodine_pregnant <- as.character(iodine_pregnant)
  return(iodine_pregnant)
}

#' @export
#' @rdname detect_iodine
#'
detect_iodine_lactating <- function(x){
  x <- stats::median(x, na.rm = FALSE)
  iodine_lactating <- cut(x,
                             breaks = c(-Inf, 100, Inf),
                             labels = c("insufficient", "adequate"),
                             right = FALSE)
  iodine_lactating <- as.character(iodine_lactating)
  return(iodine_lactating)
}


################################################################################
