#' Extract dew point temperature.
#'
#' Function extracts a dew point temperature value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A dew point temperature in degrees Celsius.
#'
#' @export
#'
#' @examples
#' metar_dew_point("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dew_point("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_dew_point("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_dew_point <- function(x) {
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  fT <- str_detect(x, pattern = "/M[\\d]+\\s")
  out[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "/M[\\d]+\\s"), 3, 4)) * -1
  fT <- str_detect(x, pattern = "/[\\d]+\\s")
  out[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "/[\\d]+\\s"), 2, 3))
  fT <- str_detect(x, pattern = "T\\d{4}0\\d{3}")
  out[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "T\\d{4}0\\d{3}"), 7, 9)) / 10.0
  fT <- str_detect(x, pattern = "T\\d{4}1\\d{3}")
  out[fT] <- -1.0 * as.numeric(str_sub(str_extract(x[fT], pattern = "T\\d{4}1\\d{3}"), 7, 9)) / 10.0
  out
}
