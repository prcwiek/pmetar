#' Extract hour and minutes.
#'
#' Function extract an hour and minutes from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A character vector HH:mm.
#'
#' @export
#'
#' @examples
#' metar_hour("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_hour("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_hour("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_hour <- function(x){
  mt <- str_extract(x, pattern = one_or_more(WRD) %R% SPC %R% one_or_more(DGT) %R% ANY_CHAR %R% SPC)
  paste(str_sub(mt, 8, 9), str_sub(mt, 10, 11), sep = ":")
}
