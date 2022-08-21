#' Get dew point temperature.
#'
#' Extracts a dew point temperature value from a METAR weather report or reports.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param check logical; if TRUE the syntax of METAR reports will be checked.
#'
#' @return a numeric vector with a dew point temperature in Celsius degrees.
#'
#' @export
#'
#' @examples
#' metar_dew_point("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dew_point("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_dew_point("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_dew_point("METAR KEWR 010851Z 27010KT 10SM FEW030 BKN070 BKN100 BKN210 04/M03 A2969")
#' metar_dew_point("201905121244 METAR KDCA 121244Z 05010KT 14/12 A2978 RMK P0002 T01390122")
#'
metar_dew_point <- function(x, check = FALSE) {
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
  # Look for the second part of the pattern nn/Mnn, M for below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "\\d{2}/M\\d{2}+\\s")
  outx[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\d{2}/M\\d{2}\\s"), 5, 6))
  # Look for the second part of the pattern nn/nn, over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "\\d{2}/\\d{2}\\s")
  outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\d{2}/\\d{2}\\s"), 4, 5))
  # Check if a more detailed temperature value is present in the other section of METAR, Tnnnn0nnn over 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(T\\d{4}0\\d{3}\\s|T\\d{4}0\\d{3}$)")
  outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(T\\d{4}0\\d{3}\\s|T\\d{4}0\\d{3}$)"), 7, 9)) / 10.0
  # Check if a more detailed temperature value is present in the other section of METAR, Tnnnn1nnn below 0 Celsisus
  fT <- stringr::str_detect(x, pattern = "(T\\d{4}1\\d{3}\\s|T\\d{4}1\\d{3}$)")
  outx[fT] <- -1.0 * as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(T\\d{4}1\\d{3}\\s|T\\d{4}1\\d{3}$)"), 7, 9)) / 10.0
  out[which(icorrect)] <- outx
  out
}
