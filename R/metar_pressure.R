#' Extract pressure.
#'
#' Function extracts an air pressure value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. An air pressure in hPa.
#'
#' @export
#'
#' @examples
#' metar_pressure("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_pressure("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_pressure("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_pressure <- function(x){
  pressure <- c(1:length(x))
  fP <- str_detect(x, pattern = "Q[\\d]+")
  pressure[fP] <- as.numeric(str_sub(str_extract(x[fP], pattern = "Q[\\d]+"), 2, 5))
  fP <- str_detect(x, pattern = "A[\\d]+")
  pressure[fP] <- as.numeric(str_sub(str_extract(x[fP], pattern = "A[\\d]+"), 2, 5)) * 0.3386389
  pressure
}
