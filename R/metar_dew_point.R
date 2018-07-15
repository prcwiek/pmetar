#' Extract dew point temperature.
#'
#' Function extract a dew point temperature value from METAR weather report.
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
  outtemp <- c(1:length(x))
  outtemp[1:length(x)] <- -273
  fT <- str_detect(x, pattern = "/M[\\d]+\\s")
  outtemp[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "/M[\\d]+\\s"), 3, 4)) * -1
  fT <- str_detect(x, pattern = "/[\\d]+\\s")
  outtemp[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "/[\\d]+\\s"), 2, 3))
  outtemp
}
