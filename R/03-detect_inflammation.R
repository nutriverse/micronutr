################################################################################
#
#'
#' Determine inflammation status
#'
#' Given laboratory values for serum c-reactive protein (CRP) and/or serum
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
#' @param label Logical. Should labels be used to classify inflammation status?
#'   If TRUE (default), status is classified as "no inflammation" or
#'   "inflammation" based on either CRP or AGP or status is classified as
#'   "no inflammation", "incubation", "early convalescence", or
#'   "late convalescence" based on both CRP and AGP. If FALSE, simple integer
#'   codes are returned: 0 for no inflammation and 1 for inflammation based on
#'   either CRP or AGP; 0 for no inflammation, 1 for incubation, 2 for early
#'   convalescence, or 3 for late convalescence.
#'
#' @return If `label` is TRUE, a character value or character vector of
#'   inflammation classification based on c-reactive protein (CRP) and/or
#'   alpha(1)-acid-glycoprotein (AGP) values. If `label` is FALSE, an integer
#'   value or vector of inflammation classification.
#'
#' @examples
#' ## Detect inflammation by AGP
#' detect_inflammation_agp(2)
#' detect_inflammation_agp(2, label = FALSE)
#'
#' ## Detect inflammation by CRP
#' detect_inflammation_crp(2)
#' detect_inflammation(crp = mnData$crp)
#' detect_inflammation(crp = mnData$crp, label = FALSE)
#'
#' ## Detect inflammation by AGP and CRP
#' detect_inflammation(crp = 2, agp = 2)
#' detect_inflammation(crp = 2, agp = 2, label = FALSE)
#'
#' @author Nicholus Tint Zaw and Ernest Guevarra
#'
#' @export
#' @rdname detect_inflammation
#'
#
################################################################################

detect_inflammation <- function(crp = NULL, agp = NULL, label = TRUE) {
  ## Check whether both CRP and AGP are NULL
  if (is.null(crp) & is.null(agp)) {
    stop("Either serum CRP or serum AGP is required to assess inflammation.")
  }

  ## Check if only CRP value available/provided
  if (!is.null(crp) & is.null(agp)) {
    ## Check if CRP is numeric
    if (is.numeric(crp)) {
      inflammation <- detect_inflammation_crp(crp = crp, label = label)
    } else {
      stop("Serum CRP should be numeric. Please try again.")
    }
  }

  ## Check if only AGP value available/provided
  if (is.null(crp) & !is.null(agp)) {
    ## Check if AGP is numeric
    if (is.numeric(agp)) {
      inflammation <- detect_inflammation_agp(agp = agp, label = label)
    } else {
      stop("Serum AGP should be numeric. Please try again.")
    }
  }

  ## Check if both CRP and AGP are available/provided
  if (!is.null(crp) & !is.null(agp)) {
    if (is.numeric(crp) & is.numeric(agp)) {
      if (label) {
        inflammation <- ifelse(crp > 5 & agp <= 1, "incubation",
          ifelse(crp <= 5 & agp > 1, "late convalescence",
            ifelse(crp > 5 & agp > 1, "early convalescence", "no inflammation")
          )
        )
      } else {
        inflammation <- ifelse(crp > 5 & agp <= 1, 1,
          ifelse(crp <= 5 & agp > 1, 3,
            ifelse(crp > 5 & agp > 1, 2, 0)
          )
        )
      }
    } else {
      stop("Serum CRP and/or serum AGP should be numeric. Please try again")
    }
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

detect_inflammation_crp <- function(crp = NULL, label = TRUE) {
  ## Check if CRP value is NULL
  if (is.null(crp)) {
    stop("Serum CRP is required to assess inflammation. Please try again.")
  }

  if (is.numeric(crp)) {
    if (label) {
      inflammation <- ifelse(crp > 5, "inflammation", "no inflammation")
    } else {
      inflammation <- ifelse(crp > 5, 1, 0)
    }
  } else {
    stop("Serum CRP should be numeric. Please try again.")
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

detect_inflammation_agp <- function(agp = NULL, label = TRUE) {
  ## Check if AGP is NULL
  if (is.null(agp)) {
    stop("Serum AGP is required to assess inflammation. Please try again.")
  }

  if (is.numeric(agp)) {
    if (label) {
      inflammation <- ifelse(agp > 1, "inflammation", "no inflammation")
    } else {
      inflammation <- ifelse(agp > 1, 1, 0)
    }
  } else {
    stop("Serum AGP should be numeric. Please try again.")
  }

  ## Return
  inflammation
}

