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
#'   micrograms per litre (microgram/l).
#' @param agp A numeric value or numeric vector for serum
#'   alpha(1)-acid-glycoprotein in micrograms per litre (microgram/l).
#' @param ferritin A numeric value or numeric vector for serum ferritin in
#'   micrograms per litre (microgram/l).
#'
#' @returns A numeric value or numeric vector for corrected serum ferritin in
#'   micrograms per litre (microgram/l).
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

