#' Extract visibility.
#'
#' Function extract a visibility from METAR weather report.
#'
#' @param x Input character vector.
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' metar_visibility("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_visibility("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_visibility("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_visibility <- function(x) {
  # correction needed!!!
  pattern_ap <- SPC %R% one_or_more(DGT) %R% SPC
  ap <- str_extract(x, pattern = pattern_ap)
  as.numeric(str_sub(ap, 2, 5))
}
