################################################################################
#
#'
#' Determine altitude correction factor for haemoglobin
#'
#' @param alt Altitude, in metres, above sea level
#'
#' @return A numeric value or numeric vector of correction factor/s for
#'   haemoglobin based on altitude
#'
#' @examples
#' get_altitude_correction(mnData$altitude)
#'
#' @author Ernest Guevarra
#'
#' @export
#'
#
################################################################################

get_altitude_correction <- function(alt = NULL) {
  if (is.null(alt)) {
    stop("Altitude required. Please try again.")
  }

  if (!is.numeric(alt)) {
    stop("Altitude should be numeric. Please try again.")
  }

  alt_factor <- cut(
    alt,
    breaks = c(-Inf, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 4500, Inf),
    labels = c(0, -2, -5, -8, -13, -19, -27, -35, -45),
    right = FALSE
  )
  alt_factor <- as.numeric(as.character(alt_factor))

  ## Return
  alt_factor
}


################################################################################
#
#'
#' Determine smoking status correction factor for haemoglobin
#'
#' @param smoke Simple integer code for smoking status classification:
#'   0 = non-smoker; 1 = any smoking or half packet up to less than 1 packet a
#'   day; 2 = 1 up to less than 2 packets a day; 3 = 2 or more packets a day.
#'
#' @return A numeric value or numeric vector of correction factor/s for
#'   haemoglobin based on smoking status
#'
#' @examples
#' get_smoking_correction(smoke = 1)
#'
#' @author Ernest Guevarra
#'
#' @export
#'
#
################################################################################

get_smoking_correction <- function(smoke = NULL) {
  if (is.null(smoke)) {
    stop("Smoking status required. Please try again.")
  }

  if (!is.double(smoke) | smoke < 0 | smoke > 3) {
    stop(
      "Smoking status should be an integer from 0 up to 3. Please try again."
    )
  }

  smoke_factor <- cut(
    smoke,
    breaks = c(-Inf, 0, 1, 2, 3),
    labels = c(0, -0.3, -0.5, -0.7),
    right = TRUE
  )
  smoke_factor <- as.numeric(as.character(smoke_factor))

  ## Return
  smoke_factor
}


################################################################################
#
#'
#' Correct haemoglobin based on altitude and/or smoking status
#'
#' @param hb A numeric value or numeric vector of serum haemoglobin
#'   concentration/s in grams per litre (g/l)
#' @param alt Altitude, in metres, above sea level
#' @param smoke Simple integer code for smoking status classification:
#'   0 = non-smoker; 1 = any smoker or half packet up to less than 1 packet a
#'   day; 2 = 1 up to less than 2 packets a day; 3 = 2 or more packets a day.
#'
#' @return A numeric value or numeric vector of corrected serum haemoglobin
#'   concentration/s in grams per litre (g/l)
#'
#' @examples
#' correct_hb(hb = mnData$hb[1], alt = mnData$altitude[1], smoke = 1)
#'
#' @author Ernest Guevarra
#'
#' @export
#'
#
################################################################################

correct_hb <- function(hb = NULL, alt = NULL, smoke = NULL) {
  if (is.null(hb)) {
    stop("Serum haemoglobin required. Please try again.")
  }

  if (!is.numeric(hb)) {
    stop("Serum haemoglobin should be numeric. Please try again.")
  }

  if (is.null(alt) & is.null(smoke)) {
    warning(
      "Altitude and smoking status not provided. Returning uncorrected haemoglobin."
    )

    alt_factor <- 0
    smoke_factor <- 0
  }

  if (is.null(alt) & !is.null(smoke)) {
    warning(
      "Altitude not provided. Returning haemoglobin corrected for smoking status only."
    )

    alt_factor <- 0
    smoke_factor <- get_smoking_correction(smoke = smoke)
  }

  if (!is.null(alt) & is.null(smoke)) {
    warning(
      "Smoking status not provided. Returning haemoglobin corrected for altitude only."
    )

    alt_factor <- get_altitude_correction(alt = alt)
    smoke_factor <- 0
  }

  if (!is.null(alt) & !is.null(smoke)) {
    smoke_factor <- get_smoking_correction(smoke = smoke)

    alt_factor <- get_altitude_correction(alt = alt)
  }

  ## Correct Hb
  corrected_hb <- hb + alt_factor + smoke_factor

  ## Return
  corrected_hb
}

