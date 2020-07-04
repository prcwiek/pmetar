#' Extract airport ICAO code.
#'
#' Extracts an airport ICAO code from METAR weather report.
#'
#' @param x character; METAR weather report or reports.
#'
#' @return A character vector with an airport ICAO code.
#'
#' @export
#'
#' @examples
#' metar_airport("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_airport("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_airport("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_airport <- function(x) {
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  fT <- stringr::str_detect(x, pattern = "(^[\\d]+(?: METAR| SPECI)|^METAR|^SPECI)")
  out[fT] <- stringr::str_sub(stringr::str_extract(x[fT], pattern = "(?:METAR|SPECI)\\s\\w{4}"), -4, -1)
  fT <- stringr::str_detect(x, pattern = "^\\w{4}\\s")
  out[fT] <- stringr::str_sub(stringr::str_extract(x[fT], pattern = "^\\w{4}\\s"), 1, 4)
  out
}
