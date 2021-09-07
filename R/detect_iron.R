################################################################################
#
#' Determining Iron Storage Status in individual and Population
#'
#' Identification of individual iron storage status on the basis of serum
#' ferritin concentration
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
#' @rdname detect_iron
#'
################################################################################


detect_iron_u5 <- function(df, ferritin = NULL, sex = NULL, app = c("crp", "agp"),
                           crp = NULL, agp = NULL){

  iron_storage_u5 <- vector(mode = "character", length = nrow(df))

  # if non
  iron_storage_u5 <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")

  #
  if("crp" %in% app){

    df$ferritin <- ifelse(df$crp > 5, (df$ferritin * 0.77), df$ferritin)
    iron_storage_u5 <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")
  }
  return(iron_storage_u5)
}



