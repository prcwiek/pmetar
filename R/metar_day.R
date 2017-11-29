#' Extract day.
#'
#' Function extract a day of a month from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector.
#'
#' @export
#'
#' @examples
#' metar_day("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_day("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_day("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_day <- function(x){
  pattern_t <- one_or_more(DGT) %R% ANY_CHAR %R% SPC
  mt <- str_extract(x, pattern = pattern_t)
  as.numeric(str_sub(mt, 1, 2))
}
