#' Get dew point temperature.
#'
#' Extracts a dew point temperature value from a METAR weather report or reports.
#'
#' @param x character vector; a METAR weather report or reports.
#'
#' @return a numeric vector with a dew point temperature in Celsius degrees.
#'
#' @export
#'
#' @examples
#' metar_dew_point("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dew_point("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_dew_point("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_dew_point("METAR KEWR 010851Z 27010KT 10SM FEW030 BKN070 BKN100 BKN210 04/M03 A2969")
#'
metar_dew_point <- function(x) {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("ERROR: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # Look for the second part of the pattern nn/Mnn, M for below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "/M[\\d]+\\s")
  out[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "/M\\d{2}\\s"), 3, 4))
  # Look for the second part of the pattern nn/nn, M for over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "/[\\d]+\\s")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "/\\d{2}\\s"), 2, 3))
  # Check if a more detailed temperature value is present in the other section of METAR, Tnnnn0nnn over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "T\\d{4}0\\d{3}")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "T\\d{4}0\\d{3}"), 7, 9)) / 10.0
  # Check if a more detailed temperature value is present in the other section of METAR, Tnnnn1nnn below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "T\\d{4}1\\d{3}")
  out[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "T\\d{4}1\\d{3}"), 7, 9)) / 10.0
  out
}
