#' Get atmospheric pressure.
#'
#' Extract and parse an air pressure value from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param altimeter boolean; if TRUE pressure is returned in inHg (inch of mercury),
#' for the default value of FALSE in hPa.
#'
#' @return a numeric vector with air pressure in inHg or hPa.
#'
#' @export
#'
#' @examples
#' metar_pressure("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_pressure("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005", altimeter = TRUE)
#' metar_pressure("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_pressure <- function(x, altimeter = FALSE){
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("ERROR: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  if(!altimeter){
    cf_hPa <- 1
    cf_inHg <- 0.3386389
  } else {
    cf_hPa <- 0.0295333727
    cf_inHg <- 0.01
  }
  pressure <- c(1:length(x))
  pressure <- NA
  fP <- stringr::str_detect(x, pattern = "\\sQ\\d{4}\\s")
  pressure[fP] <- round(as.numeric(stringr::str_sub(stringr::str_extract(x[fP], pattern = "\\sQ\\d{4}\\s"), 3, 6)) * cf_hPa, 2)
  fP <- stringr::str_detect(x, pattern = "\\sA\\d{4}\\s")
  pressure[fP] <- round(as.numeric(stringr::str_sub(stringr::str_extract(x[fP], pattern = "\\sA\\d{4}\\s"), 3, 6)) * cf_inHg, 2)
  pressure
}
