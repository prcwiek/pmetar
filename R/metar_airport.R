#' Extract airport name.
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
#' metar_airport("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_airport("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_airport("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_airport <- function(x) {
  if(str_detect(x, pattern = START %R% one_or_more(DGT) %R% " METAR")){
    out <- str_extract(x, pattern = "METAR " %R% one_or_more(WRD))
    str_sub(out, nchar(out)-3, nchar(out))
  } else {
    ap <- str_extract(x, pattern = START %R% one_or_more(WRD) %R% SPC)
    str_sub(ap, 1, 4)
  }
}
