#' Get temperature.
#'
#' Extract a temperature value from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param check logical; if TRUE the syntax of METAR reports will be checked.
#'
#' @return a numeric vector with temperature in degrees Celsius.
#'
#' @export
#'
#' @examples
#' metar_temp("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_temp("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_temp("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG=")
#' metar_temp("METAR KEWR 010851Z 27010KT 10SM BKN210 04/M03 A2969 RMK SLP054
#' T00391033 52012")
#'
metar_temp <- function(x, check = FALSE){
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  # check syntax
  if (check) {
    icorrect <- metar_is_correct(x)
  } else {
    icorrect <- rep(TRUE, length(x))
  }
  x <- x[which(icorrect)]
  outx <- c(1:length(x))
  outx[c(1:length(x))] <- NA
  # Look for the first part of the pattern nn/.., M for over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(\\s\\d{2}/\\d{2}|\\s\\d{2}/M\\d{2})")
  outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(\\s\\d{2}/\\d{2}|\\s\\d{2}/M\\d{2})"), 2, 3))
  # Look for the first part of the pattern Mnn/..., M for below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(M\\d{2}/\\d{2}|M\\d{2}/M\\d{2})")
  outx[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(M\\d{2}/\\d{2}|M\\d{2}/M\\d{2})"), 2, 3))
  # Check if a more detailed temperature value is present in the other section of METAR, T0nnnnnnnn over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(T0[\\d]{3}\\d{4}\\s|T0[\\d]{3}\\d{4}$)")
  outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(T0[\\d]{3}\\d{4}\\s|T0[\\d]{3}\\d{4}$)"), 3, 5)) / 10.0
  # Check if a more detailed temperature value is present in the other section of METAR, T1nnnnnnnn below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(T1[\\d]{3}\\d{4}\\s|T1[\\d]{3}\\d{4}$)")
  outx[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(T1[\\d]{3}\\d{4}\\s|T1[\\d]{3}\\d{4}$)"), 3, 5)) / 10.0
  out[which(icorrect)] <- outx
  out
}
