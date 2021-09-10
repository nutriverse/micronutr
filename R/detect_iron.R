################################################################################
#
#' @title Determining Iron Storage Status in individual
#'
#' @description Identification of individual iron storage status on the basis of
#' serum ferritin concentration
#'
#'
#' This package includes two main components of functions; (1) to identify the
#' individual iron storage status for each specific target group (for example,
#' for under 5 children or the children at 5 years old or older), and (2) to
#' identify the individual iron storage for the general population. In the
#' first component of the package, you can use the specific type of command to
#' identify the iron storage status of your specific interested population.
#' If your interested sample population is children under 5 years old, you can
#' use `detect_iron_u5` commend. If your target is the children at 5 years old
#' or older (including adults), the commend called `detect_iron_over5` is
#' available. The below-detailed parameter description session explained the
#' detailed requirement of each parameter to execute those commands. In the
#' second component, the commend for the general population, you can use the
#' `detect_iron` commend. This is the instrumental one of your study population
#' mixed with different interest groups (including both under 5 years old
#' children and over). This will help you execute the function in one commend
#' line and generate one variable to indicate the study dataset's individual
#' observation iron storage status.
#'
#' For overall population function;
#'
#' The arguments applied in this overall population function is almost identical
#' with the individual commends:`detect_iron_u5` and `detect_iron_over5`. The
#' only exception is this overall function has one additional argument, which is
#' `age_group`.
#'
#'
#' @param df Survey dataset (as an R data.frame) with the presence of serum
#'    ferritin variable recorded in (µg/l) uint. The following variables were
#'    also required to apply function commend thoroughly, but those are not
#'    compulsory.
#'
#'    | **Variable** | **Description** |
#'    | :--- | :--- |
#'    | child's sex |	indicate the sample observation is male or female |
#'    | CRP |	acute phase response proteine: C-reactive protein (CRP) value |
#'    | AGP	| acute phase response proteine: α1-acid-glycoprotein (AGP) value |
#'
#' @param ferritin Sample observation's serum ferritin level from the dataset.
#'    The values should record in the micrograms per liter (µg/l) unit.
#'
#' @param sex This parameter indicates the observation's sex category, coded as
#'    `1` for males and `2` for females. However, for this child under 5 years
#'    old population, this parameter is not mandatory. The cut-off point
#'    applied in the classification of iron status for children with under 5
#'    years population was not affected by the child's sex status. Therefore,
#'    this argument is not a mendatory one for the function `detect_iron_u5`.
#'    However, if you apply for the children at 5 years or older (including
#'    adults population), this argument is a mandatory for `detect_iron_over5`
#'    commend.
#'
#' @param app The serum ferritin level is affected by the presence of
#'    inflammation and infection. If you want to correct the serum ferritin
#'    value in the presence of inflammation in the study population, you can
#'    apply this parameter. This vector indicates the specific acute phase
#'    response protein you want to include in the process of serum ferritin
#'    value correction. There were two main vector names you can apply in this
#'    parameter which were "agp" (α1-acid-glycoprotein (AGP)) and "crp"
#'    (C-reactive protein (CRP)). You can use either a single vector or both in
#'    this parameter assignment. Based on your input protein(s) type(s), the
#'    function will identify the inflammation stages of each observation in the
#'    dataset. Then, based on the inflammation stage, the respective correction
#'    values will apply. The detailed classification of inflammation stages is
#'    mentioned in the below table.
#'
#'    | **inflammation Categories** | **Cut-off Points** | **Correction Values** |
#'    | :--- | :--- | :--- |
#'    | Incubation | CRP only: CRP > 5 mg/L | 0.77 |
#'    | Early convalescence | CRP & AGP: CRP > 5 mg/L and AGP > 1 g/L | 0.53 |
#'    | Late convalescence | AGP only: AGP > 1 g/L | 0.75 |
#'    Multiple the serumn ferritin avlue (µg/l) by the number mentioned in the
#'    correction values column.
#'
#' @param crp After mentioning the type of acute phase response protein in the
#'    `app` parameter, you need to identify the variable name of each protein
#'    type in your dataset. If you mentioned `crp` in the `app` argument, you
#'    need to specify the variable's name, which contains the "C-reactive
#'    protein (CRP)" value (mg/L) in your dataset here.
#'
#' @param agp If you mentioned `agp` in the `app` argument, you need to specify
#'    the variable's name, which contains the "α1-acid-glycoprotein (AGP)" value
#'    (g/L) in your dataset here.
#'
#' @param add If you want to add the new generated variable `iron_storage` to
#'    identify each dataset observation's iron storage status, please apply
#'    `TRUE` in this argument, which is also the default option. The new
#'    variable `iron_storage` has three distinct values for iron storage status;
#'    deficiency, no deficiency, and iron overload. The cut-off points value for
#'    each category is mentioned in the below table.
#'
#'    | **Population** | **No Deficiency** |	**Deficiency** | **Iron Overload** |
#'    | :--- | :--- | :--- | :--- |
#'    | < 5 year old (both sex) |	Ferritin < 12 |	Ferritin >= 12 | NA  |
#'    | >= 5 years old (Male) |	Ferritin < 15	|  15 >= Ferritin >= 200 |	Ferritin > 200 |
#'    | >= 5 years old (Female)	| Ferritin < 15	| 15 >= Ferritin >= 150 |	Ferritin > 150 |
#'
#'
#' @param age_group The variable name that identifies the type of target
#'    population included in the dataset was required to execute this function
#'    properly. It would be best to assign two numeric values in this variable
#'    which were coded as `1` for "5 years and older" and `2` for "under 5
#'    years". In this argument, you only need to mention that age group assigned
#'    variable name (from your dataset) in the commend.
#'
#'
#' @return iron_storage A data frame with the same structure as `df` is named
#'    `iron_storage`. In this data.frame, the new variable called `iron_storage`
#'    contains the result of iron storage status for each observation in the
#'    dataset.
#'
#' @examples
#'
#'  # For individual target group function;
#'  ## U5 children
#'  ## no ferritin correction
#'   detect_iron_u5(df = ferritin_sample, ferritin = ferritin, add = TRUE)
#'
#'  ## ferritin correction (CRP only)
#'   detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
#'                      app = "crp", crp = crp, add = TRUE)
#'
#'  ## ferritin correction (AGP only)
#'   detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
#'                      app = "agp", crp = agp, add = TRUE)
#'
#'  ## ferritin correction (AGP and CRP combined)
#'   detect_iron_u5(df = ferritin_sample, ferritin = ferritin,
#'                      app = c("crp", "agp"), crp = crp,
#'                      agp = agp, add = TRUE)
#'
#'
#'  ## 5 years old and older
#'  ## no ferritin correction
#'   detect_iron_over5(df = ferritin_sample, ferritin = ferritin,
#'                       sex = sex, add = TRUE)
#'
#'  ## ferritin correction (CRP only)
#'   detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
#'                       app = "crp", crp = crp, add = TRUE)
#'
#'  ## ferritin correction (AGP only)
#'   detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
#'                       app = "agp", crp = agp, add = TRUE)
#'
#'  ## ferritin correction (AGP and CRP combined)
#'   detect_iron_over5(df = ferritin_sample, ferritin = ferritin, sex = sex,
#'                       app = c("crp", "agp"), crp = crp,
#'                       agp = agp, add = TRUE)
#'
#'
#'
#'  # For overall population function;
#'  ## no ferritin correction
#'   detect_iron(df = ferritin_sample, ferritin = ferritin,
#'                      sex = sex, age_group = age_group,
#'                      add = TRUE)
#'
#'  ## ferritin correction (CRP only)
#'   detect_iron(df = ferritin_sample, ferritin = ferritin,
#'                      sex = sex, age_group = age_group,
#'                      app = "crp", crp = crp, add = TRUE)
#'
#'  ## ferritin correction (AGP only)
#'   detect_iron(df = ferritin_sample, ferritin = ferritin,
#'                      sex = sex, age_group = age_group,
#'                      app = "agp", agp = agp, add = TRUE)
#'
#'  ## ferritin correction (AGP and CRP combined)
#'   detect_iron(df = ferritin_sample, ferritin = ferritin,
#'                      sex = sex, age_group = age_group,
#'                      app = c("crp", "agp"), crp = crp, agp = agp,
#'                      add = TRUE)
#'
#'
#' @export
#'
#' @rdname detect_iron
#'
#'
#################################################################################

# Overall fucntion for all population
detect_iron <- function(df, ferritin = NULL, sex = NULL, age_group = NULL,
                       app = NULL, crp = NULL, agp = NULL, add = TRUE){

  iron_storage <- vector(mode = "character", length = nrow(df))

  # if both agp and crp were mentioned
  if("crp" %in% app & "crp" %in% names(df) &
     "agp" %in% app & "agp" %in% names(df)){

    df$ferritin <- ifelse(df$crp > 1 & df$crp > 5,
                          (df$ferritin * 0.53), df$ferritin)
    iron_storage <- ifelse((df$ferritin < 15 & df$age_group == 1) |
                                   (df$ferritin < 12 & df$age_group == 2), "deficiency",
                                 ifelse((df$ferritin > 200 & df$age_group == 1 & df$sex == 1), "iron overload",
                                        ifelse(df$ferritin > 150 & df$age_group == 1 & df$sex == 2, "iron overload",
                                               "no deficiency")))
  }

  # if crp was mentioned
  if("crp" %in% app & "crp" %in% names(df) & (
    !("agp" %in% names(df)) |
    !("agp" %in% app))){

    df$ferritin <- ifelse(df$crp > 5, (df$ferritin * 0.77), df$ferritin)
    iron_storage <- ifelse((df$ferritin < 15 & df$age_group == 1) |
                             (df$ferritin < 12 & df$age_group == 2), "deficiency",
                           ifelse((df$ferritin > 200 & df$age_group == 1 & df$sex == 1), "iron overload",
                                  ifelse(df$ferritin > 150 & df$age_group == 1 & df$sex == 2, "iron overload",
                                         "no deficiency")))
  }


  # if agp was mentioned
  if("agp" %in% app & "agp" %in% names(df) & (
    !("crp" %in% names(df)) |
    !("crp" %in% app))){

    df$ferritin <- ifelse(df$agp > 1, (df$ferritin * 0.75), df$ferritin)
    iron_storage <- ifelse((df$ferritin < 15 & df$age_group == 1) |
                             (df$ferritin < 12 & df$age_group == 2), "deficiency",
                           ifelse((df$ferritin > 200 & df$age_group == 1 & df$sex == 1), "iron overload",
                                  ifelse(df$ferritin > 150 & df$age_group == 1 & df$sex == 2, "iron overload",
                                         "no deficiency")))
  }

  # if both agp and crp were not  mentioned
  if(missing(app) & missing(agp) & missing(crp)){

    iron_storage <- ifelse((df$ferritin < 15 & df$age_group == 1) |
                             (df$ferritin < 12 & df$age_group == 2), "deficiency",
                           ifelse((df$ferritin > 200 & df$age_group == 1 & df$sex == 1), "iron overload",
                                  ifelse(df$ferritin > 150 & df$age_group == 1 & df$sex == 2, "iron overload",
                                         "no deficiency")))
  }

  ##
  if(add) {
    iron_storage <- data.frame(df, iron_storage)
  }

  return(iron_storage)
}


################################################################################
#' @export
#' @rdname detect_iron
#'

# Children Under 5 years old
detect_iron_u5 <- function(df, ferritin = NULL, sex = NULL, app = NULL,
                           crp = NULL, agp = NULL, add = TRUE){

  iron_storage <- vector(mode = "character", length = nrow(df))

  # if both agp and crp were mentioned
  if("crp" %in% app & "crp" %in% names(df) &
     "agp" %in% app & "agp" %in% names(df)){

    df$ferritin <- ifelse(df$crp > 1 & df$crp > 5,
                          (df$ferritin * 0.53), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")
  }

  # if crp was mentioned
  if("crp" %in% app & "crp" %in% names(df) & (
    !("agp" %in% names(df)) |
    !("agp" %in% app))){

    df$ferritin <- ifelse(df$crp > 5, (df$ferritin * 0.77), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")
  }


  # if agp was mentioned
  if("agp" %in% app & "agp" %in% names(df) & (
    !("crp" %in% names(df)) |
    !("crp" %in% app))){

    df$ferritin <- ifelse(df$agp > 1, (df$ferritin * 0.75), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")
  }

  # if both agp and crp were not  mentioned
  if(missing(app) & missing(agp) & missing(crp)){

    iron_storage <- ifelse(df$ferritin < 12, "deficiency", "no deficiency")
  }

  ##
  if(add) {
    iron_storage <- data.frame(df, iron_storage)
  }

  return(iron_storage)
}

################################################################################
#' @export
#' @rdname detect_iron
#'

# Children 5 years old and older

detect_iron_over5 <- function(df, ferritin = NULL, sex = NULL, app = NULL,
                           crp = NULL, agp = NULL, add = TRUE){

  iron_storage <- vector(mode = "character", length = nrow(df))

  # if both agp and crp were mentioned
  if("crp" %in% app & "crp" %in% names(df) &
     "agp" %in% app & "agp" %in% names(df)){


    df$ferritin <- ifelse(df$crp > 1 & df$crp > 5,
                          (df$ferritin * 0.53), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 15, "deficiency",
                                 ifelse(df$ferritin > 200 & df$sex == 1, "iron overload",
                                        ifelse(df$ferritin > 150 & df$sex == 2, "iron overload",
                                               "no deficiency")))
  }

  # if crp was mentioned
  if("crp" %in% app & "crp" %in% names(df) & (
    !("agp" %in% names(df)) |
    !("agp" %in% app))){

    df$ferritin <- ifelse(df$crp > 5, (df$ferritin * 0.77), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 15, "deficiency",
                                 ifelse(df$ferritin > 200 & df$sex == 1, "iron overload",
                                        ifelse(df$ferritin > 150 & df$sex == 2, "iron overload",
                                               "no deficiency")))
  }


  # if agp was mentioned
  if("agp" %in% app & "agp" %in% names(df) & (
    !("crp" %in% names(df)) |
    !("crp" %in% app))){

    df$ferritin <- ifelse(df$agp > 1, (df$ferritin * 0.75), df$ferritin)
    iron_storage <- ifelse(df$ferritin < 15, "deficiency",
                                 ifelse(df$ferritin > 200 & df$sex == 1, "iron overload",
                                        ifelse(df$ferritin > 150 & df$sex == 2, "iron overload",
                                               "no deficiency")))
  }

  # if both agp and crp were not  mentioned
  if(missing(app) & missing(agp) & missing(crp)){

    iron_storage <- ifelse(df$ferritin < 15, "deficiency",
                                 ifelse(df$ferritin > 200 & df$sex == 1, "iron overload",
                                        ifelse(df$ferritin > 150 & df$sex == 2, "iron overload",
                                               "no deficiency")))
  }

  ##
  if(add) {
    iron_storage <- data.frame(df, iron_storage)
  }

  return(iron_storage)
}



