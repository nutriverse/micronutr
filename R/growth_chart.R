################################################################################
#
#'
#' Extract weight-for-length/height z-score data
#'
#' Function to extract weight-for-length/height z-score data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for weight-for-length/height z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 4 coloumns and 21636 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{lh}}{Length/height of child in centimetres.}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Weight (kgs) value for the specified \code{sd_type}}
#' }
#'
#' @examples
#' # Get weight-for-length/height z-score expanded tables for charting
#' #get_wfh_zchart()
#'
#'
#
################################################################################

get_wfh_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                           gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data_2 <- read.table(file = paste(baseurl, "wfl_", i, "_z_exp.txt", sep = ""),
                           header = TRUE)
    z_data_5 <- read.table(file = paste(baseurl, "wfh_", i, "_z_exp.txt", sep = ""),
                           header = TRUE)
    ##
    z_data_2 <- data.frame("sex" = i, z_data_2)
    names(z_data_2) <- c("sex", "lh", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                         "1SD", "2SD", "3SD", "4SD")
    ##
    z_data_5 <- data.frame("sex" = i, z_data_5)
    names(z_data_5) <- c("sex", "lh", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                         "1SD", "2SD", "3SD", "4SD")
    ##
    z_data   <- data.frame(rbind(z_data_2, z_data_5))

    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "lh", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                   "1SD", "2SD", "3SD", "4SD")
  ##
  wfh_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                             names(temp)[3]:names(temp)[ncol(temp)])
  names(wfh_chart) <- c("sex", "lh", "sd_type", "sd_value")
  wfh_chart$sd_type <- factor(wfh_chart$sd_type,
                              levels = c("4SD", "3SD", "2SD", "1SD", "0", "-1SD",
                                         "-2SD", "-3SD", "-4SD"))
  ##
  wfh_chart
}


################################################################################
#
#'
#' Extract weight-for-length/height percentile data
#'
#' Function to extract weight-for-length/height percentile data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for weight-for-length/height percentiles.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 7 coloumns and 36060 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{lh}}{Length/height (in centimetres) of child in months.}
#' \item{\code{l}}{\code{L} component of the LMS method for normalising growth
#'     centile standards. \code{L} is the trend in the optimal power to obtain
#'     normality}
#' \item{\code{m}}{\code{M} component of the LMS method for normalising growth
#'     centile standards. \code{M} is the trend in the mean}
#' \item{\code{s}}{\code{S} component of the LMS method for normalising growth
#'     centile standards. \code{S} is the trend in the coefficient of variation}
#' \item{\code{p_type}}{Type of z-score. Can be one of \code{0.10th}, \code{1st},
#'     \code{3rd}, \code{5th}, \code{10th}, \code{15th}, \code{25th}, \code{50th},
#'     \code{75th}, \code{85th}, \code{90th}, \code{95th}, \code{97th},
#'     \code{99th} and \code{99.9th}}
#' \item{\code{p_value}}{Weight (kgs) value for the specified \code{p_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get weight-for-length/height percentile expanded tables for charting
#' #get_wfh_pchart()
#'
#
################################################################################

get_wfh_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                           gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data_2 <- read.table(file = paste(baseurl, "wfl_", i, "_p_exp.txt", sep = ""),
                           header = TRUE)
    p_data_5 <- read.table(file = paste(baseurl, "wfh_", i, "_p_exp.txt", sep = ""),
                           header = TRUE)
    ##
    p_data_2 <- data.frame("sex" = i, p_data_2)
    names(p_data_2) <- c("sex", "lh", "l", "m", "s",
                         "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                         "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")
    p_data_5 <- data.frame("sex" = i, p_data_5)
    names(p_data_5) <- c("sex", "lh", "l", "m", "s",
                         "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                         "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")
    ##
    p_data <- data.frame(rbind(p_data_2, p_data_5))
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "lh", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")
  ##
  wfh_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                             names(temp)[6]:names(temp)[ncol(temp)])
  names(wfh_chart) <- c("sex", "lh", "l", "m", "s", "p_type", "p_value")
  wfh_chart$p_type <- factor(wfh_chart$p_type,
                             levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                        "15th", "25th", "50th", "75th", "85th",
                                        "90th", "95th", "97th", "99th", "99.9th"))
  ##
  wfh_chart
}


################################################################################
#
#'
#' Extract weight-for-age z-score data
#'
#' Function to extract weight-for-age z-score data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for weight-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 5 coloumns and 33426 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Weight (kgs) value for the specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get weight-for-age z-score expanded tables for charting
#' #get_wfa_zchart()
#'
#
################################################################################

get_wfa_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                           gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data <- read.table(file = paste(baseurl, "wfa_", i, "_z_exp.txt", sep = ""),
                         header = TRUE)
    ##
    z_data <- data.frame("sex" = i, "month" = z_data$Day/30.4375, z_data)
    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                   "1SD", "2SD", "3SD", "4SD")
  ##
  wfa_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                             names(temp)[4]:names(temp)[ncol(temp)])
  names(wfa_chart) <- c("sex", "month", "day", "sd_type", "sd_value")
  wfa_chart$sd_type <- factor(wfa_chart$sd_type,
                              levels = c("4SD", "3SD", "2SD", "1SD", "0",
                                         "-1SD", "-2SD", "-3SD", "-4SD"))
  ##
  #wfa_chart
}


################################################################################
#
#'
#' Extract weight-for-age percentile data
#'
#' Function to extract weight-for-age percentile data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for weight-for-age percentiles.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 8 coloumns and 55710 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{l}}{\code{L} component of the LMS method for normalising growth
#'     centile standards. \code{L} is the trend in the optimal power to obtain
#'     normality}
#' \item{\code{m}}{\code{M} component of the LMS method for normalising growth
#'     centile standards. \code{M} is the trend in the mean}
#' \item{\code{s}}{\code{S} component of the LMS method for normalising growth
#'     centile standards. \code{S} is the trend in the coefficient of variation}
#' \item{\code{p_type}}{Type of z-score. Can be one of \code{0.10th}, \code{1st},
#'     \code{3rd}, \code{5th}, \code{10th}, \code{15th}, \code{25th}, \code{50th},
#'     \code{75th}, \code{85th}, \code{90th}, \code{95th}, \code{97th},
#'     \code{99th} and \code{99.9th}}
#' \item{\code{p_value}}{Weight (kgs) value for the specified \code{p_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get weight-for-age percentile expanded tables for charting
#' #get_wfa_pchart()
#'
#
################################################################################

get_wfa_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                           gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data <- read.table(file = paste(baseurl, "wfa_", i, "_p_exp.txt", sep = ""),
                         header = TRUE)
    ##
    p_data <- data.frame("sex" = i, "month" = p_data$Age/30.4375, p_data)
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")
  ##
  wfa_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                             names(temp)[7]:names(temp)[ncol(temp)])
  names(wfa_chart) <- c("sex", "month", "day", "l", "m", "s", "p_type", "p_value")
  wfa_chart$p_type <- factor(wfa_chart$p_type,
                             levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                        "15th", "25th", "50th", "75th", "85th",
                                        "90th", "95th", "97th", "99th", "99.9th"))

  ##
  wfa_chart
}


################################################################################
#
#'
#' Extract triceps skinfold-for-age z-score data
#'
#' Function to extract triceps skinfold-for-age z-score data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for tricpes skinfold-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 5 coloumns and 31788 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Tricepts skinfold (in centimetres) value for the
#'     specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get triceps skinfold-for-age z-score expanded tables for charting
#' #get_tsfa_zchart()
#'
#
################################################################################

get_tsfa_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data <- read.table(file = paste(baseurl, "second_set/tsfa_", i, "_z_exp.txt", sep = ""),
                         header = TRUE)
    ##
    z_data <- data.frame("sex" = i, "month" = z_data$Day/30.4375, z_data)
    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                   "1SD", "2SD", "3SD", "4SD")

  ##
  tsfa_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                              names(temp)[4]:names(temp)[ncol(temp)])
  names(tsfa_chart) <- c("sex", "month", "day", "sd_type", "sd_value")
  tsfa_chart$sd_type <- factor(tsfa_chart$sd_type,
                               levels = c("4SD", "3SD", "2SD", "1SD", "0",
                                          "-1SD", "-2SD", "-3SD", "-4SD"))

  ##
  tsfa_chart
}


################################################################################
#
#'
#' Extract triceps skinfold-for-age percentiles data
#'
#' Function to extract triceps skinfold-for-age percentiles data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for triceps skinfold-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 8 coloumns and 52980 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Triceps skinfold (in centimetres) value for the
#'     specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get triceps skinfold-for-age percentiles expanded tables for charting
#' #get_tsfa_pchart()
#'
#
################################################################################

get_tsfa_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data <- read.table(file = paste(baseurl, "second_set/tsfa_", i, "_p_exp.txt", sep = ""),
                         header = TRUE)
    ##
    p_data <- data.frame("sex" = i, "month" = p_data$Age/30.4375, p_data)
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")

  ##
  tsfa_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                              names(temp)[7]:names(temp)[ncol(temp)])
  names(tsfa_chart) <- c("sex", "month", "day", "l", "m", "s", "p_type", "p_value")
  tsfa_chart$p_type <- factor(tsfa_chart$p_type,
                              levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                         "15th", "25th", "50th", "75th", "85th",
                                         "90th", "95th", "97th", "99th", "99.9th"))

  ##
  tsfa_chart
}


################################################################################
#
#'
#' Extract subscapular skinfold-for-age z-score data
#'
#' Function to extract subscapular skinfold-for-age z-score data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for subscapular skinfold-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 5 coloumns and 31788 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Subscapular skinfold (in centimetres) value for the specified
#'     \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get subscapular skinfold-for-age z-score expanded tables for charting
#' #get_ssfa_zchart()
#'
#
################################################################################

get_ssfa_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data <- read.table(file = paste(baseurl, "second_set/ssfa_", i, "_z_exp.txt", sep = ""),
                         header = TRUE)
    ##
    z_data <- data.frame("sex" = i, "month" = z_data$Day/30.4375, z_data)
    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                   "1SD", "2SD", "3SD", "4SD")

  ##
  ssfa_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                              names(temp)[4]:names(temp)[ncol(temp)])
  names(ssfa_chart) <- c("sex", "month", "day", "sd_type", "sd_value")
  ssfa_chart$sd_type <- factor(ssfa_chart$sd_type,
                               levels = c("4SD", "3SD", "2SD", "1SD", "0",
                                          "-1SD", "-2SD", "-3SD", "-4SD"))

  ##
  ssfa_chart
}


################################################################################
#
#'
#' Extract subscapular skinfold-for-age percentiles data
#'
#' Function to extract subscapular skinfold-for-age percentiles data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for subscapular skinfold-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 8 coloumns and 52980 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Subscapular skinfold (in centimetres) value for the
#'     specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get subscapular skinfold-for-age percentiles expanded tables for charting
#' #get_ssfa_pchart()
#'
#
################################################################################

get_ssfa_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data <- read.table(file = paste(baseurl, "second_set/ssfa_", i, "_p_exp.txt", sep = ""),
                         header = TRUE)
    ##
    p_data <- data.frame("sex" = i, "month" = p_data$Age/30.4375, p_data)
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")

  ##
  ssfa_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                              names(temp)[7]:names(temp)[ncol(temp)])
  names(ssfa_chart) <- c("sex", "month", "day", "l", "m", "s", "p_type", "p_value")
  ssfa_chart$p_type <- factor(ssfa_chart$p_type,
                              levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                         "15th", "25th", "50th", "75th", "85th",
                                         "90th", "95th", "97th", "99th", "99.9th"))

  ##
  ssfa_chart
}


################################################################################
#
#'
#' Extract length/height-for-age z-score data
#'
#' Function to extract length/height-for-age z-score data from the World Health
#' Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for length/height-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 5 coloumns and 33426 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Length/height value for the specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get length/height-for-age z-score expanded tables for charting
#' #get_lhfa_zchart()
#'
#
################################################################################

get_lhfa_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data <- read.table(file = paste(baseurl, "lhfa_", i, "_z_exp.txt", sep = ""),
                         header = TRUE)
    ##
    z_data <- data.frame("sex" = i, "month" = z_data$Day/30.4375, z_data)
    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "-4SD", "-3SD", "-2SD", "-1SD",
                   "0", "1SD", "2SD", "3SD", "4SD")

  ##
  lhfa_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                              names(temp)[4]:names(temp)[ncol(temp)])
  names(lhfa_chart) <- c("sex", "month", "day", "sd_type", "sd_value")
  lhfa_chart$sd_type <- factor(lhfa_chart$sd_type,
                               levels = c("4SD", "3SD", "2SD", "1SD", "0",
                                          "-1SD", "-2SD", "-3SD", "-4SD"))

  ##
  lhfa_chart
}



################################################################################
#
#'
#' Extract length/height-for-age percentiles data
#'
#' Function to extract length/height-for-age percentiles data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for length/height-for-age percentiles.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 8 coloumns and 55710 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{l}}{\code{L} component of the LMS method for normalising growth
#'     centile standards. \code{L} is the trend in the optimal power to obtain
#'     normality}
#' \item{\code{m}}{\code{M} component of the LMS method for normalising growth
#'     centile standards. \code{M} is the trend in the mean}
#' \item{\code{s}}{\code{S} component of the LMS method for normalising growth
#'     centile standards. \code{S} is the trend in the coefficient of variation}
#' \item{\code{p_type}}{Type of z-score. Can be one of \code{0.10th}, \code{1st},
#'     \code{3rd}, \code{5th}, \code{10th}, \code{15th}, \code{25th}, \code{50th},
#'     \code{75th}, \code{85th}, \code{90th}, \code{95th}, \code{97th},
#'     \code{99th} and \code{99.9th}}
#' \item{\code{p_value}}{Length/height value for the specified \code{p_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get length/height-for-age percentile expanded tables for charting
#' #get_lhfa_pchart()
#'
#
################################################################################

get_lhfa_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data <- read.table(file = paste(baseurl, "lhfa_", i, "_p_exp.txt", sep = ""),
                         header = TRUE)
    ##
    p_data <- data.frame("sex" = i, "month" = p_data$Day/30.4375, p_data)
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")

  ##
  lhfa_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                              names(temp)[7]:names(temp)[ncol(temp)])
  names(lhfa_chart) <- c("sex", "month", "day", "l", "m", "s", "p_type", "p_value")
  lhfa_chart$p_type <- factor(lhfa_chart$p_type,
                              levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                         "15th", "25th", "50th", "75th", "85th",
                                         "90th", "95th", "97th", "99th", "99.9th"))

  ##
  lhfa_chart
}


################################################################################
#
#'
#' Extract head circumference-for-age z-scores data
#'
#' Function to extract head circumference-for-age z-scores data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for head circumference-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 5 coloumns and 33426 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Head circumference (in centimetres) value for the
#'     specified \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get head circumference-for-age z-score expanded tables for charting
#' #get_hcfa_zchart()
#'
#
################################################################################

get_hcfa_zchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    z_data <- read.table(
      file = paste(baseurl, "second_set/hcfa_", i, "_z_exp.txt", sep = ""),
      header = TRUE
    )
    ##
    z_data <- data.frame("sex" = i, "month" = z_data$Day/30.4375, z_data)
    ##
    temp <- data.frame(rbind(temp, z_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "-4SD", "-3SD", "-2SD", "-1SD", "0",
                   "1SD", "2SD", "3SD", "4SD")

  ##
  hcfa_chart <- tidyr::gather(data = temp, key = "sd_type", value = "sd_value",
                              names(temp)[4]:names(temp)[ncol(temp)])
  names(hcfa_chart) <- c("sex", "month", "day", "sd_type", "sd_value")
  hcfa_chart$sd_type <- factor(hcfa_chart$sd_type,
                               levels = c("4SD", "3SD", "2SD", "1SD", "0",
                                          "-1SD", "-2SD", "-3SD", "-4SD"))

  ##
  hcfa_chart
}


################################################################################
#
#'
#' Extract head circumference-for-age percentiles data
#'
#' Function to extract head circumference-for-age percentiles data from the
#' World Health Organization (WHO) website on Child Growth Standards
#' \url{http://www.who.int/childgrowth/standards/"} which contains a
#' collection of reference data from the WHO Multicentre Growth Reference Study
#' (MGRS). This function extracts the expanded data provided by WHO for creating
#' growth charts for head circumference-for-age z-scores.
#'
#' @param baseurl Character value for the base URL of the WHO Child Growth
#'     Standards. The default is \url{http://www.who.int/childgrowth/standards/}.
#'     This can be adjusted should WHO change the base URL for the Child Growth
#'     Standards section of their website.
#' @param gender A character value or vector that specifies the sex-specific
#'     reference data to extract. The default is \code{c("boys", "girls")}.
#'
#' @return A data frame in tidy format with 8 coloumns and 55710 rows:
#' \describe{
#' \item{\code{sex}}{Sex of child. 1 = boy; 2 = girl}
#' \item{\code{month}}{Age of child in months. This is calculated from the
#'     original data using that data for age of child in days}
#' \item{\code{day}}{Age of child in days. This is provided for by default in
#'     the original data}
#' \item{\code{sd_type}}{Type of z-score. Can be one of \code{4SD}, \code{3SD},
#'     \code{2SD}, \code{1SD}, \code{0}, \code{-1SD}, \code{-2SD}, \code{-3SD},
#'     \code{-4SD}}
#' \item{\code{sd_value}}{Head circumference (in centimetres) value for the specified
#'     \code{sd_type}}
#' }
#'
#' @author Ernest Guevarra
#'
#' @examples
#' # Get head circumference-for-age percentiles expanded tables for charting
#' #get_hcfa_pchart()
#'
#
################################################################################

get_hcfa_pchart <- function(baseurl = "http://www.who.int/childgrowth/standards/",
                            gender = c("boys", "girls")) {
  ##
  temp <- NULL

  ##
  for(i in gender) {
    ##
    p_data <- read.table(file = paste(baseurl, "second_set/hcfa_", i, "_p_exp.txt", sep = ""),
                         header = TRUE)
    ##
    p_data <- data.frame("sex" = i, "month" = p_data$Age/30.4375, p_data)
    ##
    temp <- data.frame(rbind(temp, p_data))
  }

  ##
  names(temp) <- c("sex", "month", "day", "l", "m", "s",
                   "0.10th", "1st", "3rd", "5th", "10th", "15th", "25th", "50th",
                   "75th", "85th", "90th", "95th", "97th", "99th", "99.9th")

  ##
  hcfa_chart <- tidyr::gather(data = temp, key = "p_type", value = "p_value",
                              names(temp)[7]:names(temp)[ncol(temp)])
  names(hcfa_chart) <- c("sex", "month", "day", "l", "m", "s", "p_type", "p_value")
  hcfa_chart$p_type <- factor(hcfa_chart$p_type,
                              levels = c("0.10th", "1st", "3rd", "5th", "10th",
                                         "15th", "25th", "50th", "75th", "85th",
                                         "90th", "95th", "97th", "99th", "99.9th"))

  ##
  hcfa_chart
}

