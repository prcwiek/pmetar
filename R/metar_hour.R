#' Extract hour and minutes.
#'
#' Function extracts an hour and minutes from METAR weather report.
#'
#' @param x character; a METAR weather report or reports.
#'
#' @return A character vector with the METAR time in the format HH:mm.
#'
#' @export
#'
#' @examples
#' metar_hour("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_hour("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_hour("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_hour <- function(x){
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # look for nnnnnn[A-Z], like 281830Z
  fT <- stringr::str_detect(x, pattern = "\\w{4}\\s\\d{6}[A-Z]")
  out[fT] <- stringr::str_extract(x[fT], pattern = "\\w{4}\\s\\d{6}[A-Z]")
  out[fT] <- paste(stringr::str_sub(out[fT], -5, -4), stringr::str_sub(out[fT], -3, -2), sep = ":")
  out
}
