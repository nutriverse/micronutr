################################################################################
#
#'
#' Serum haemoglobin values for women and children
#'
#' This sample dataset contains 18,487 observations and 9 variables and is
#' described below.
#'
#' @format A data frame with 9 columns and 18487 rows.
#'
#' **Variables** | **Description**
#' :--- | :---
#' *psu* | Primary sampling unit identifier
#' *state* | State identifier
#' *locality* | Locality identifier
#' *sex* | Sex (1 = Male; 2 = Female)
#' *m.age* | Age (in years) of female/mother with ages > 5 years
#' *ch.age* | Age (in months) of children with ages less than 5 years
#' *group* | Group classification
#' *hb* | Haemoglobin values (g/L)
#' *altitude* | Altitude of sampling location above sea level (metres)
#'
#' @source From a micronutrient survey in a country in East Africa
#'
#' @examples
#' # explore the first 20 observations from the dataset
#' head(haemoglobin, 20)
#'
#
################################################################################
"haemoglobin"


################################################################################
#
#'
#' Urinary iodine (μg/L) values for women and children
#'
#' This sample dataset contains 312 observations and 8 variables.
#'
#' @format A data frame with 8 columns and 312 rows.
#'
#' **Variables** | **Description**
#' :--- | :---
#' *psu* | Primary sampling unit identifier
#' *state* | State identifier
#' *locality* | Locality identifier
#' *sex* | Sex (1 = Male; 2 = Female)
#' *m.age* | Age (in years) of female/mother with ages > 5 years
#' *ch.age* | Age (in months) of children with ages less than 5 years
#' *group* | Group classification
#' *iodine* | Urinary iodine (μg/L) values
#'
#' @source From a micronutrient survey in a country in East Africa
#'
#' @examples
#' # explore the first 20 observations from the dataset
#' head(iodine, 20)
#'
#
################################################################################
"iodine"

################################################################################
#
#'
#' Serum ferritin (µg/l) level sample dataset
#'
#' This sample dataset contains 19449 observations and 10 variables.
#'
#' @format A data frame with 10 columns and 19449 rows.
#'
#' **Variables** | **Description**
#' :--- | :---
#' *psu* | Primary sampling unit identifier
#' *state* | State identifier
#' *locality* | Locality identifier
#' *sex* | Sex (1 = Male; 2 = Female)
#' *m.age* | Age (in years) of female/mother with ages > 5 years
#' *ch.age* | Age (in months) of children with ages less than 5 years
#' *group* | Group classification
#' *crp* | acute phase response proteine: C-reactive protein (CRP) value (mg/L)
#' *ferritin* | Serum ferritin (μg/L) values
#' *agp* | acute phase response proteine: α1-acid-glycoprotein (AGP) value (g/L)
#' *age_group* | indicate study population (Under 5 or over 5 years old)
#'
#' @source From a micronutrient survey in a country in East Africa
#'
#' @examples
#' # explore the first 20 observations from the dataset
#' head(ferritin, 20)
#'
#
################################################################################
"ferritin"

