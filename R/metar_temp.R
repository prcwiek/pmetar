#' Extract temperature.
#'
#' Function extract a temperature value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A temperature in degrees Celsius.
#'
#' @export
#'
#' @examples
#' metar_temp("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_temp("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_temp("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_temp <- function(x){
  pattern_temp <- "M" %R% one_or_more(DGT) %R% "/"
  if(str_detect(x, pattern = pattern_temp)) {
    temp <- str_extract(x, pattern = pattern_temp)
    as.numeric(str_sub(temp, 2, 3)) * -1
  } else {
    pattern_temp <- one_or_more(DGT) %R% "/" # %R% one_or_more(DGT)
    temp <- str_extract(x, pattern = pattern_temp)
    as.numeric(str_sub(temp, 1, 2))
  }
}
