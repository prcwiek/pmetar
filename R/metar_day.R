#' Extract day of month.
#'
#' Extracts a day of a month from METAR weather report.
#'
#' @param x character; a METAR weather report or reports.
#'
#' @return A numeric vector with a day of a month.
#'
#' @export
#'
#' @examples
#' metar_day("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_day("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_day("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_day <- function(x){
  if(x[1] != ""){
    if(stringr::str_detect(x, pattern = "[\\d]+.\\s")[1] & !stringr::str_detect(x, pattern = "^[\\d]+\\s[\\w]+\\s[\\w]+\\s[\\d]+.\\s")[1]){
      out <- stringr::str_extract(x, pattern = "[\\d]+.\\s")
      as.numeric(stringr::str_sub(out, 1, 2))
    } else {
      out <- stringr::str_extract(x, pattern = "^[\\d]+\\s[\\w]+\\s[\\w]+\\s[\\d]+.\\s")
      as.numeric(stringr::str_sub(out, 25, 26))
    }
  } else {
    cat("Argument missing!\n")
  }
}
