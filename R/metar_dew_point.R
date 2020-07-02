#' Extract dew point temperature.
#'
#' Extracts a dew point temperature value from a METAR weather report or reports.
#'
#' @param x character; a METAR weather report or reports.
#'
#' @return A numeric vector with a dew point temperature in Celsius degrees.
#'
#' @export
#'
#' @examples
#' metar_dew_point("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dew_point("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_dew_point("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#' metar_dew_point("202001010851 METAR KEWR 010851Z 27010KT 10SM FEW030 BKN070 BKN100 BKN210 04/M03 A2969 RMK SLP054 T00391033 52012")
#'
metar_dew_point <- function(x) {
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  fT <- stringr::str_detect(x, pattern = "/M[\\d]+\\s")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "/M\\d{2}\\s"), 3, 4)) * -1
  fT <- stringr::str_detect(x, pattern = "/[\\d]+\\s")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "/\\d{2}\\s"), 2, 3))
  fT <- stringr::str_detect(x, pattern = "T\\d{4}0\\d{3}")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "T\\d{4}0\\d{3}"), 7, 9)) / 10.0
  fT <- stringr::str_detect(x, pattern = "T\\d{4}1\\d{3}")
  out[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "T\\d{4}1\\d{3}"), 7, 9)) / 10.0
  out
}
