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
#'   crp = ferritin$crp, agp = ferritin$agp, ferritin = ferritin$ferritin
#' )
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
  inflammation <- detect_inflammation(crp = crp, agp = agp)

  ## Correct inflammation based on crp
  if (!is.null(crp) & is.null(agp)) {
    ferritin_corrected <- ifelse(
      inflammation == "inflammation", ferritin * 0.65, ferritin
    )
  }

  ## Correct ferritin based on agp
  if (is.null(crp) & !is.null(agp)) {
    ferritin_corrected <- ifelse(
      inflammation == "inflammation", ferritin * 0.72, ferritin
    )
  }

  ## Correct ferritin based on crp and agp
  if (!is.null(crp) & !is.null(agp)) {
    ferritin_corrected <- ifelse(inflammation == "incubation", ferritin * 0.77,
      ifelse(inflammation == "late cnvalescence", ferritin * 0.53,
        ifelse(inflammation == "early convalescence", ferritin * 0.75, ferritin)
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
#'   micrograms per liter (µg/l).
#' @param group A character value specifying the population target group to
#'   determine iron status from. Can be either for under 5 year old ("u5") or
#'   5 years and over ("5over"). Default to "u5".
#' @param inflammation Logical value or vector. Is subject in inflammation or
#'   not?
#'
#' @return A character value or character vector of iron status. Can be either
#'   "iron deficiency" or "no iron deficiency".
#'
#'
#' @examples
#'  # Iron storage status based on CRP only
#'  ferritin_corrected <- correct_ferritin(
#'    crp = ferritin$crp, ferritin = ferritin$ferritin
#'  )
#'  detect_iron_deficiency(ferritin_corrected)
#'
#'  # Iron storage status based on AGP only
#'  ferritin_corrected <- correct_ferritin(
#'    agp = ferritin$agp, ferritin = ferritin$ferritin
#'  )
#'  detect_iron_deficiency(ferritin_corrected)
#'
#'  # Iron storage status based on CRP and AGP
#'  ferritin_corrected <- correct_ferritin(
#'    crp = ferritin$crp, agp = ferritin$agp, ferritin = ferritin$ferritin
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
#' @export
#' @rdname detect_iron_deficiency
#'
#'
#
################################################################################

detect_iron_deficiency_u5 <- function(ferritin = NULL) {
  ## Check that ferritin is not NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Determine iron status
  iron_status <- ifelse(ferritin < 12, "iron deficiency", "no iron deficiency")

  ## Return
  iron_status
}


################################################################################
#
#' @export
#' @rdname detect_iron_deficiency
#
################################################################################

detect_iron_deficiency_5over <- function(ferritin = NULL) {
  ## Check that ferritin is not NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  ## Determine iron status
  iron_status <- ifelse(ferritin < 15, "iron deficiency", "no iron deficiency")

  ## Return
  iron_status
}


################################################################################
#
#' @export
#' @rdname detect_iron_deficiency
#
################################################################################

detect_iron_deficiency <- function(ferritin = NULL, group = c("u5", "5over")) {
  ## Check if ferritin is NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Get group
  group <- match.arg(group)

  ## Determine iron status
  iron_status <- eval(
    parse(text = paste0("detect_iron_deficiency_", group, "(ferritin)"))
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
  iron_status <- mapply(
    FUN = detect_iron_deficiency_qualitative_,
    ferritin = ferritin,
    inflammation = inflammation,
    SIMPLIFY = TRUE
  )

  ## Return
  iron_status
}


################################################################################
#
# Function to vectorise over - not for export
#
################################################################################

detect_iron_deficiency_qualitative_ <- function(ferritin = NULL,
                                                inflammation = NULL) {
  ## Check if ferritin is NULL
  if (is.null(ferritin)) {
    stop("Serum ferritin required. Please try again.")
  }

  ## Check that serum ferritin is numeric
  if (!is.numeric(ferritin)) {
    stop("Serum ferritin should be numeric. Please try again.")
  }

  if (inflammation) {
    iron_status <- ifelse(ferritin < 30, "iron deficiency", "no irondeficiency")
  } else {
    iron_status <- NA
  }

  ## Return
  iron_status
}


