#' Extract time zone.
#'
#' Function extract a name of airport from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' metar_time_zone("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_time_zone("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_time_zone("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_time_zone <- function(x){
  mt <- str_extract(x, pattern = "[\\d]+[A-Z]")
  pUTC <- str_detect(mt, pattern = "Z")
  mt[pUTC] <- "UTC"
  mt[!pUTC] <- "local time"
  mt
}
