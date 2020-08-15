#' Get day of month.
#'
#' Extract a day of a month from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#'
#' @return a numeric vector with a day of a month.
#'
#' @export
#'
#' @examples
#' metar_day("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_day("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_day("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#'
metar_day <- function(x){
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("ERROR: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # look for nnnnnn[A-Z], like 281830Z
  fT <- stringr::str_detect(x, pattern = "\\w{4}\\s\\d{6}[A-Z]")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\w{4}\\s\\d{6}[A-Z]"), -7, -6))
  out
}
