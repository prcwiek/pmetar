#' Get airport ICAO, International Civil Aviation Organization, code.
#'
#' Extract an airport ICAO code from METAR weather report.
#'
#' @param x character vector; METAR weather report or reports.
#'
#' @return a character vector with an airport ICAO code.
#'
#' @export
#'
#' @examples
#' metar_airport("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_airport("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_airport("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_airport("202103251800 METAR COR NFTL 251800Z 00000KT SCT017TCU BKN290 25/25 Q1014")
#'
metar_airport <- function(x) {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  fT <- stringr::str_detect(x, pattern = "(^[\\d]+(?: METAR| SPECI| METAR COR)|^METAR|^SPECI|^METAR COR)")
  out[fT] <- stringr::str_sub(stringr::str_extract(x[fT], pattern = "(?:METAR|SPECI|METAR COR)\\s[A-Z]{4}"), -4, -1)
  fT <- stringr::str_detect(x, pattern = "^\\w{4}\\s")
  out[fT] <- stringr::str_sub(stringr::str_extract(x[fT], pattern = "^[A-Z]{4}\\s"), 1, 4)
  out[is.na(out)] <- "Incorrect METAR!"
  out
}
