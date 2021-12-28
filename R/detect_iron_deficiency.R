################################################################################
#
#' @title Determining Iron Storage Status in individual
#'
#' @description Identification of individual iron storage status on the basis of
#' serum ferritin concentration
#'
#'
#' This package includes three main series of functional components:
#'   (1) Identifying inflammation level of the individual observation
#'   based on the available type of acute-phase proteins.
#'   (2) Correcting the ferritin value based on the inflammation status.
#'   (3) Categorizing the iron storage status of individual observation
#'   based on the corrected ferritin value of the specific target group
#'   (for example, for under 5 children or the children at 5 years old or
#'   older).
#'
#'
#' In the first component, you can use the specific type of command to identify
#' the inflammation stages based on the availability of acute-phase proteins
#' (one of either C-reactive protein (CRP) or α1-acid-glycoprotein (AGP), or
#' both).
#'  `def_crp`:  If your dataset has CRP protein only or if you want to apply
#'  CRP protein alone for the identification of inflammation status, use this
#'  command. Based on the CRP values, the command generates a vector name
#'  `inflammation` ,which holds the binary outcome ("inflammation" and
#'  "No Inflammation").
#'  `def_agp`: If you only want to apply AGP protein, this command is designed
#'  for it. Based on the AGP values, the command generates a vector name
#'  `inflammation` ,which holds the binary outcome ("inflammation" and
#'  "No Inflammation").
#'  `detect_inflammation`: If your dataset has both CRP and AGP proteins or
#'  if you want to apply both proteins in the identification of inflammation
#'  status, use this command. Based on the cut-off value of each
#'  protein's values, the command generates a vector name `inflammation`,
#'  which holds the categorical outcome with 4 different categories
#'  ("No Inflammation", "Incubation", "Late Convalescence", and
#'  "Early Convalescence").
#'
#'
#'  The detailed classification of inflammation status based on combination of
#'  both CRP and AGP proteins apply the following cut-off values criteria
#'  mentioned in the below table.
#'
#'    | **Inflammation Categories** | **Cut-off Points** |
#'    | :--- | :--- |
#'    | Incubation | CRP > 5 mg/L & AGP <= 1 g/L |
#'    | Early convalescence | CRP > 5 mg/L & AGP > 1 g/L |
#'    | Late convalescence | CRP <= 5 mg/L & AGP > 1 g/L |
#'
#'  If only either CRP or AGP proteins is applied, the cut-off points for each
#'  type of protein are mentioned in the below table.
#'
#'    | **Type of Protein** | **Cut-off Points (inflammation)** |
#'    | :--- | :--- |
#'    | CRP | CRP > 5 mg/L |
#'    | AGP | AGP > 1 g/L |
#'
#'
#' Then, in the second stage, based on the inflammation status result from step
#' one, there is a specific commend for ferritin value correction and each
#' command will provide the new vector called `ferritin_corrected` with the
#' fixed ferritin value.
#'  `correct_ferritin`: use this command if the first stage of identification of
#'  inflammation was performed based on both acute-phase proteins (CRP and AGP)
#'  `correct_ferritin_crp`: If the inflammation status of your data was
#'  identified by CRP protein alone, use this command for correction of ferritin
#'  value. This command will correct each observation ferritin value respective
#'  to their inflammation status.
#'  `correct_ferritin_agp`: If the identification of inflammation status was
#'  performed by the AGP protein alone, use this command. This command will
#'  correct the ferritin value based on the respective observation inflammation
#'  status.
#'  `correct_ferritin`: If your data observation inflammation status was
#'  four-category outputs as both CRP and AGP protein identified those, use
#'  this one. Based on each observation inflammation status, each observation
#'  ferritin values were corrected.
#'
#'  If the inflammation status was identified based on combination of both CRP
#'  and AGP proteins, the ferritin correction values are performed as multiply
#'  with the correction values provided in the below table.
#'
#'    | **inflammation Categories** | **Correction Values** |
#'    | :--- | :--- |
#'    | Incubation | 0.77 |
#'    | Early convalescence | 0.53 |
#'    | Late convalescence | 0.75 |
#'
#'  If only either CRP or AGP proteins is applied, the different correction
#'  value are applied based on type of protein as mentioned in the below table.
#'
#'    | **Type of Protein** | **Correction Values** |
#'    | :--- | :--- |
#'    | CRP | 0.65 |
#'    | AGP | 0.72 |
#'
#'
#' In the final stage, based on the corrected ferritin value and specific target
#' age group, there were particular commends to perform iron storage status
#' diagnosis for individual observation.
#'  `detect_iron`: If your data had already corrected ferritin value based on
#'  either acute-phase protein or both, use this command. Based on the age group
#'  of the individual observation and its inflammation status, this will
#'  generate the new vector called `iron_storage` and which contains the binary
#'  outcome of iron storage status ("deficiency" and "no deficiency"). This
#'  command works for both under 5 and over 5 years old.
#'  `detect_iron_quali`: If the inflammation status was identified based on the
#'  qualitative report (such as self-reporting from the survey), use this
#'  commend for iron status identification. This will also generate the new
#'  vector called `iron_storage` and which contains the binary outcome of
#'  iron storage status ("deficiency" and "no deficiency"). But, this can only
#'  apply to the under 5 years old children population.
#'
#'  The cut-off points value for identification of iron storage category is
#'  mentioned in the below table.
#'
#'    | **Population** | **No Deficiency** |	**Deficiency** |
#'    | :--- | :--- | :--- | :--- |
#'    | < 5 year old (both sex) |	Ferritin < 12 |	Ferritin >= 12 |
#'    | >= 5 years old (Male) |	Ferritin < 15	|  15 >= Ferritin >= 200 |
#'    | >= 5 years old (Female)	| Ferritin < 15	| 15 >= Ferritin >= 150 |
#'
#'  If the inflammation status was identified based on qualitative inforomation,
#'  the cut-off point is applied to original ferritin value as < 30 micrograms
#'  per liter (µg/l) unit is consider as `deficiency` status.
#'
#'
#' @param ferritin Sample observation's serum ferritin level from the dataset.
#'    The values should record in the micrograms per liter (µg/l) unit.
#'
#' @param crp If you mentioned `crp` in the argument, you need to specify
#'    the variable's name, which contains the "C-reactive protein (CRP)" value
#'    (mg/L) in your dataset or vector you want to apply.
#'
#' @param agp If you mentioned `agp` in the argument, you need to specify
#'    the variable's name, which contains the "α1-acid-glycoprotein (AGP)" value
#'    (g/L) in your dataset or vector you want to apply.
#'
#' @param inflammation This argument holds information about the inflammation
#'    status. Based on either of the acute-phase protein or both in identifying
#'    inflammation status, the inflammation variable may contain two different
#'    types of information. If the inflammation status was identified using
#'    only one protein (either CRP or AGP), this variable must have the binary
#'    outcome as "inflammation" and "No Inflammation". If it was resulted from
#'    using both types of protein, it must contain the following category in the
#'    variable: "No Inflammation", "Incubation", "Late Convalescence", and
#'    "Early Convalescence".
#'
#' @param ferritin_corrected This argument is designed to place the corrected
#'    ferritin value (in the micrograms per liter (µg/l) unit) on the
#'    inflammation status. If you use the original ferritin value, that will
#'    result in incorrect identification of iron storage status.
#'
#' @param inflammation_quali This is the binary outcome qualitative information
#'    about inflammation either resulted from self-reporting about inflammation
#'    or infection in the survey (or other types of data collection). You may
#'    need to transform the variable before applying in this argument as a
#'    numerical binary outcome as 1"inflammation" and 0"No Inflammation".
#'
#' @param age_group The variable name that identifies the type of target
#'    population included in the dataset (or the vector name you want to apply
#'    in this argument) was required to execute this function properly. This
#'    argument should contain a value to indicate the age group status of each
#'    observation as either "under 5 years" or "5 years and older".
#'
#' @return The return will be different based on the type of sub-function we
#' used in this function package. If you use the identification of inflammation
#' commands, the return result was a vector output (named as `inflammation`)
#' with the marker of either the binary outcome on "inflammation" and
#' "No Inflammation", or a four-category character vector ("No Inflammation",
#' "Incubation", "Late Convalescence", and "Early Convalescence"). If you use
#' the ferritin correction command, the return was a vector called
#' `ferritin_corrected` which contained corrected ferritin value based on the
#' respective inflammation status. When using the iron storage identification
#' commend, the return is about `iron_storage` vector which includes the marker
#' of either "deficiency" or "no deficiency".
#'
#'
#' @examples
#'
#'  #  Identification of the inflammation status
#'  ## CRP only
#'  def_crp(ferritin$crp)
#'
#'  ## AGP only
#'  def_agp(ferritin$agp)
#'
#'  ## Both CRP and AGP
#'  detect_inflammation(ferritin$crp, ferritin$agp)
#'
#'  #  Ferritin correction
#'  #  (based on inflammation status and marker protein used in identification)
#'  ## inflammation identified by CRP only
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- def_crp(ferritin$crp)
#'  correct_ferritin_crp(ferritin$ferritin, ferritin$inflammation)
#'
#'  ## inflammation identified by AGP only
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- def_agp(ferritin$agp)
#'  correct_ferritin_agp(ferritin$ferritin, ferritin$inflammation)
#'
#'  ## inflammation identified by both CRP and AGP
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- detect_inflammation(ferritin$crp, ferritin$agp)
#'  correct_ferritin(ferritin$ferritin, ferritin$inflammation)
#'
#'  #  Iron storage status identification
#'  ## Based on inflammation status defined by acute-phase protein
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- def_crp(ferritin$crp)
#'  ferritin$ferritin_corrected <- correct_ferritin_crp(
#'    ferritin$ferritin, ferritin$inflammation
#'  )
#'  detect_iron(ferritin$ferritin_corrected, ferritin$age_group)
#'
#'  ## inflammation identified by AGP only
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- def_agp(ferritin$agp)
#'  ferritin$ferritin_corrected <- correct_ferritin_agp(
#'    ferritin$ferritin, ferritin$inflammation
#'  )
#'  detect_iron(ferritin$ferritin_corrected, ferritin$age_group)
#'
#'  ## inflammation identified by both CRP and AGP
#'  ### identify the inflammation status and perform ferritin value correction
#'  ferritin$inflammation <- detect_inflammation(
#'    ferritin$crp, ferritin$agp)
#'  ferritin$ferritin_corrected <- correct_ferritin(
#'    ferritin$ferritin, ferritin$inflammation
#'  )
#'  detect_iron(ferritin$ferritin_corrected, ferritin$age_group)
#'
#'  ## Based on the qualitative information of infection or inflammation
#'  detect_iron_quali(ferritin$ferritin, ferritin$infection)
#'
#'
#' @export
#'
#' @rdname detect_iron
#'
#'
#################################################################################

#################################################################################
# Evaluation of inflammation - overall function
#################################################################################

# Both CRP and AGP for different category
detect_inflammation <- function(crp, agp){
  inflammation <- ifelse(crp <= 5 & agp <= 1, "No Inflammation",
                         ifelse(crp > 5 & agp <= 1, "Incubation",
                                ifelse(crp <= 5 & agp > 1, "Late Convalescence",
                                       ifelse(crp > 5 & agp > 1,
                                              "Early Convalescence", NA))))

  inflammation
}


################################################################################

# Evaluation of inflammation - individual stage
# (1) elevated CRP only
#def_incubation <- function(crp, agp){
#
#  inflammation <- ifelse(crp > 5 & agp <= 1, "Incubation",
#                         "No Inflammation")
#
#  return(inflammation)
#}
# (2) elevated AGP only
#def_lateconvale <- function(crp, agp){
#
#  inflammation <- ifelse(crp <= 5 & agp > 1, "Late Convalescence",
#                        "No Inflammation")
#
#  return(inflammation)
#}
# (3) elevated both CRP and AGP
#def_earlyconvale <- function(crp, agp){
#
#  inflammation <- ifelse(crp > 5 & agp > 1, "Early Convalescence",
#                         "No Inflammation")
#
#  return(inflammation)
#}

#' @export
#' @rdname detect_iron
#'

################################################################################

# Evaluation of inflammation - by individual protein
# (1) by CRP only
def_crp <- function(crp){

  inflammation <- ifelse(crp > 5, "inflammation", "No Inflammation")

  return(inflammation)
}

#' @export
#' @rdname detect_iron
#'


# (2) by AGP only
def_agp <- function(agp){

  inflammation <- ifelse(agp > 1, "inflammation", "No Inflammation")

  ## Return
  inflammation
}


#################################################################################

#################################################################################
# Ferritin Correction
#################################################################################

# Based on the inflammation identified by individual protein
# (1) inflammation by CRP only

#' @export
#' @rdname detect_iron
#'
correct_ferritin_crp <- function(ferritin, inflammation){

  ferritin_corrected <- ifelse(
    inflammation == "inflammation", ferritin * 0.65, ferritin)

  ## Return
  ferritin_corrected
}


# (2) inflammation by AGP only

#' @export
#' @rdname detect_iron
#'
correct_ferritin_agp <- function(ferritin, inflammation){

  ferritin_corrected <- ifelse(
    inflammation == "inflammation", ferritin * 0.72, ferritin)

  ## Return
  ferritin_corrected
}


################################################################################

# Based on the different stages of inflammation

#' @export
#' @rdname detect_iron
#'
correct_ferritin <- function(ferritin, inflammation){

  ferritin_corrected <- ifelse(inflammation == "Incubation", ferritin * 0.77,
                               ifelse(inflammation == "Late Convalescence",
                                      ferritin * 0.53,
                                      ifelse(inflammation == "Early Convalescence",
                                             ferritin * 0.75, ferritin)))

  ## Return
  ferritin_corrected
}


################################################################################

################################################################################
# Iron Storage Identification
################################################################################

# detection of iron storage status based on corrected ferritin value

#' @export
#' @rdname detect_iron
#'
detect_iron <- function(ferritin_corrected, age_group){

  iron_storage <- ifelse((ferritin_corrected < 12 &
                            age_group == "under 5 years") |
                           (ferritin_corrected < 15 &
                              age_group == "5 years and older") ,
                         "deficiency",
                         ifelse(is.na(ferritin_corrected) | is.na(age_group),
                                NA, "no deficiency"))

  # Return
  iron_storage
}


################################################################################
# detection of iron storage status based on qualitative information on
# inflammation

#' @export
#' @rdname detect_iron
#'
detect_iron_quali <- function(ferritin, inflammation_quali){

  iron_storage <- ifelse(ferritin < 30 & inflammation_quali == 1,
                         "deficiency",
                         ifelse(is.na(ferritin) | is.na(inflammation_quali),
                                NA, "no deficiency"))

  ## Return
  iron_storage
}


