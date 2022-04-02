################################################################################
#
#'
#' Correct serum ferritin values
#'
#' Based on inflammation status, correct serum ferritin values as described in
#' Namaste, S. M., Rohner, F., Huang, J., Bhushan, N. L., Flores-Ayala, R.,
#' Kupka, R., Mei, Z., Rawat, R., Williams, A. M., Raiten, D. J.,
#' Northrop-Clewes, C. A., & Suchdev, P. S. (2017). Adjusting ferritin
#' concentrations for inflammation: Biomarkers Reflecting Inflammation and
#' Nutritional Determinants of Anemia (BRINDA) project. The American journal of
#' clinical nutrition, 106(Suppl 1), 359S–371S.
#' https://doi.org/10.3945/ajcn.116.141762
#'
#' @param crp A numeric value or numeric vector for serum c-reactive protein in
#'   micrograms per litre (µg/l).
#' @param agp A numeric value or numeric vector for serum
#'   alpha(1)-acid-glycoprotein in micrograms per litre (µg/l).
#' @param ferritin A numeric value or numeric vector for serum ferritin in
#'   micrograms per litre (µg/l).
#'
#' @return A numeric value or numeric vector for corrected serum ferritin in
#'   micrograms per litre (µg/l).
#'
#' @examples
#' correct_ferritin(
#'   crp = mnData$crp, ferritin = mnData$ferritin
#' )
#'
#' @author Nicholus Tint Zaw and Ernest Guevarra
#'
#' @export
#'
#
################################################################################

correct_ferritin <- function(crp = NULL, agp = NULL, ferritin = NULL) {
  ## Check if ferritin is NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check if ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Detect inflammation
  inflammation <- detect_inflammation(crp = crp, agp = agp, label = FALSE)

  ## Correct inflammation based on crp
  if (!is.null(crp) & is.null(agp)) {
    ferritin_corrected <- ifelse(
      inflammation == 1, ferritin * 0.65, ferritin
    )
  }

  ## Correct ferritin based on agp
  if (is.null(crp) & !is.null(agp)) {
    ferritin_corrected <- ifelse(
      inflammation == 1, ferritin * 0.72, ferritin
    )
  }

  ## Correct ferritin based on crp and agp
  if (!is.null(crp) & !is.null(agp)) {
    ferritin_corrected <- ifelse(inflammation == 1, ferritin * 0.77,
      ifelse(inflammation == 3, ferritin * 0.53,
        ifelse(inflammation == 2, ferritin * 0.75, ferritin)
      )
    )
  }

  ## Return
  ferritin_corrected
}


################################################################################
#
#'
#' Determine iron storage status
#'
#' Given serum ferritin values, determine iron storage status.
#'
#' @param ferritin A numeric value or numeric vector of serum ferritin level in
#'   micrograms per litre (µg/l).
#' @param group A character value specifying the population target group to
#'   determine iron status from. Can be either for under 5 year old ("u5") or
#'   5 years and over ("5over"). Default to "u5".
#' @param inflammation Logical value or vector. Is subject in inflammation or
#'   not?
#' @param label Logical. Should labels be used to classify iron storage status?
#'   If TRUE (default), status is classified as "no iron deficiency" or
#'   "iron deficiency". If FALSE, simple integer codes are returned: 0 for
#'   no iron deficiency and 1 for iron deficiency.
#'
#' @return If [label] is TRUE, a character value or character vector of iron
#'   status classification (can be either "iron deficiency" or "no iron
#'   deficiency"). If [label] is FALSE, an integer value or integer vector of
#'   iron status classification (0 = no iron deficiency; 1 = iron deficiency)
#'
#' @examples
#'  # Iron storage status based on CRP only
#'  ferritin_corrected <- correct_ferritin(
#'    crp = mnData$crp, ferritin = mnData$ferritin
#'  )
#'  detect_iron_deficiency(ferritin_corrected)
#'
#'  # Iron storage status based on AGP only
#'  ferritin_corrected <- correct_ferritin(
#'    agp = 2, ferritin = mnData$ferritin[1]
#'  )
#'  detect_iron_deficiency(ferritin_corrected)
#'
#'  # Iron storage status based on CRP and AGP
#'  ferritin_corrected <- correct_ferritin(
#'    crp = mnData$crp[1], agp = 2, ferritin = mnData$ferritin[1]
#'  )
#'  detect_iron_deficiency(ferritin_corrected)
#'
#'  # Iron storage status - qualitative
#'  detect_iron_deficiency_qualitative(
#'    ferritin = 3, inflammation = TRUE
#'  )
#'  detect_iron_deficiency_qualitative(
#'    ferritin = c(2, 3, 5), inflammation = c(TRUE, FALSE, TRUE)
#'  )
#'
#' @author Nicholus Tint Zaw and Ernest Guevarra
#'
#' @export
#' @rdname detect_iron_deficiency
#'
#'
#
################################################################################

detect_iron_deficiency_u5 <- function(ferritin = NULL, label = TRUE) {
  ## Check that ferritin is not NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Determine iron status
  if (label) {
    iron_status <- ifelse(
      ferritin < 12, "iron deficiency", "no iron deficiency"
    )
  } else {
    iron_status <- ifelse(ferritin < 12, 1, 0)
  }

  ## Return
  iron_status
}


################################################################################
#
#' @export
#' @rdname detect_iron_deficiency
#
################################################################################

detect_iron_deficiency_5over <- function(ferritin = NULL, label = TRUE) {
  ## Check that ferritin is not NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Determine iron status
  if (label) {
    iron_status <- ifelse(
      ferritin < 15, "iron deficiency", "no iron deficiency"
    )
  } else {
    iron_status <- ifelse(ferritin < 15, 1, 0)
  }

  ## Return
  iron_status
}


################################################################################
#
#' @export
#' @rdname detect_iron_deficiency
#
################################################################################

detect_iron_deficiency <- function(ferritin = NULL,
                                   group = c("u5", "5over"),
                                   label = TRUE) {
  ## Check if ferritin is NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Get group
  group <- match.arg(group)

  ## Determine iron status
  iron_status <- eval(
    parse(
      text = paste0(
        "detect_iron_deficiency_", group, "(ferritin, label = label)"
      )
    )
  )

  ## Return
  iron_status
}


################################################################################
#
#' @export
#' @rdname detect_iron_deficiency
#
################################################################################

detect_iron_deficiency_qualitative <- function(ferritin = NULL,
                                               inflammation = NULL) {
  ## Vectorise detect_iron_deficiency_qualitative
  iron_status <- Map(
    f = detect_iron_deficiency_qualitative_,
    ferritin = ferritin,
    inflammation = inflammation
  )

  ##
  iron_status <- unlist(iron_status)

  ## Return
  iron_status
}


################################################################################
#
# Function to vectorise over - not for export
#
################################################################################

detect_iron_deficiency_qualitative_ <- function(ferritin = NULL,
                                                inflammation = NULL,
                                                label = TRUE) {
  ## Check if ferritin is NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that serum ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ##
  if (!is.logical(inflammation)) {
    stop("Inflammation status should be logical. Please try again.")
  }

  if (inflammation) {
    if (label) {
      iron_status <- ifelse(
        ferritin < 30, "iron deficiency", "no iron deficiency"
      )
    } else {
      iron_status <- ifelse(ferritin < 30, 1, 0)
    }
  } else {
    if (label) {
      iron_status <- NA_character_
    } else {
      iron_status <- NA_integer_
    }
  }

  ## Return
  iron_status
}


