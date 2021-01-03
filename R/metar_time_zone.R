#' Get time zone.
#'
#' Extract a time zone of METAR weather report.
#'
#' @param x character; a METAR weather report or reports.
#'
#' @return a character vector with time zone.
#'
#' @export
#'
#' @examples
#' metar_time_zone("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_time_zone("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_time_zone("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#'
metar_time_zone <- function(x){
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # look for nnnnnn[A-Z], like 281830Z
  fT <- stringr::str_detect(x, pattern = "\\w{4}\\s\\d{6}[A-Z]")
  out[fT] <- stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\w{4}\\s\\d{6}[A-Z]"), -1, -1)
  out
}
