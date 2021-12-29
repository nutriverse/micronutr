################################################################################
#
#'
#' Determine inflammation status
#'
#' Given laboratory values for serum c-reactive proteint (CRP) and/or serum
#' alpha(1)-acid-glycoprotein (AGP), the inflammation status of a subject can
#' be determined based on cut-off values described in Namaste, S. M.,
#' Rohner, F., Huang, J., Bhushan, N. L., Flores-Ayala, R., Kupka, R., Mei, Z.,
#' Rawat, R., Williams, A. M., Raiten, D. J., Northrop-Clewes, C. A., &
#' Suchdev, P. S. (2017). Adjusting ferritin concentrations for inflammation:
#' Biomarkers Reflecting Inflammation and Nutritional Determinants of Anemia
#' (BRINDA) project. The American journal of clinical nutrition, 106(Suppl 1),
#' 359S–371S. https://doi.org/10.3945/ajcn.116.141762
#'
#' @param crp A numeric value or numeric vector of c-reactive protein (crp)
#'   values in micrograms per litre (µg/l).
#' @param agp A numeric value or numeric vector of alpha(1)-acid-glycoprotein
#'   (agp) values in micrograms per litre (µg/l).
#'
#' @return A character value or character vector of inflammation classification
#'   based on c-reactive protein (CRP) and/or alpha(1)-acid-glycoprotein (AGP)
#'   values.
#'
#' @examples
#' ## Detect inflammation by AGP
#' detect_inflammation_agp(2)
#' detect_inflammation(agp = ferritin$agp)
#'
#' ## Detect inflammation by CRP
#' detect_inflammation_crp(2)
#' detect_inflammation(crp = ferritin$crp)
#'
#' ## Detect inflammation by AGP and CRP
#' detect_inflammation(crp = 2, agp = 2)
#' detect_inflammation(crp = ferritin$crp, agp = ferritin$agp)
#'
#'
#' @export
#' @rdname detect_inflammation
#'
#
################################################################################

# detect_inflammation <- function(crp = NULL, agp = NULL) {
#   ## Vectorise detect_inflammation_both
#   inflammation <- mapply(
#     FUN = detect_inflammation_,
#     crp = crp,
#     agp = agp,
#     SIMPLIFY = TRUE
#   )
#
#   ## Return
#   inflammation
# }


################################################################################
#
# Function to vectorise over - not for export
#
################################################################################

detect_inflammation <- function(crp = NULL, agp = NULL) {
  if (is.null(crp) & is.null(agp)) {
    stop("Either crp or agp is required to assess inflammation.")
  }

  if (!is.null(crp) & is.null(agp)) {
    inflammation <- detect_inflammation_crp(crp)
  }

  if (is.null(crp) & !is.null(agp)) {
    inflammation <- detect_inflammation_agp(agp)
  }

  if (!is.null(crp) & !is.null(agp)) {
    inflammation <- ifelse(crp > 5 & agp <= 1, "incubation",
      ifelse(crp <= 5 & agp > 1, "late convalescence",
        ifelse(crp > 5 & agp > 1, "early convalescence", "no inflammation")
      )
    )
  }

  ## Return
  inflammation
}


################################################################################
#
#' @export
#' @rdname detect_inflammation
#'
#
################################################################################

detect_inflammation_crp <- function(crp) {
  inflammation <- ifelse(crp > 5, "inflammation", "no inflammation")

  ## Return
  inflammation
}


################################################################################
#
#' @export
#' @rdname detect_inflammation
#'
#
################################################################################

detect_inflammation_agp <- function(agp) {
  inflammation <- ifelse(agp > 1, "inflammation", "no inflammation")

  ## Return
  inflammation
}

