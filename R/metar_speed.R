#' Extract wind speed
#'
#' Function extract a wind speed value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A wind speed in m/s.
#'
#' @export
#'
#' @examples
#' metar_speed("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_speed("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_speed("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_speed <- function(x){
  speed <- c(1:length(x))
  speed[c(1:length(x))] <- 0
  fMPS <- str_detect(x, pattern = "\\d\\dMPS")
  fKT <- str_detect(x, pattern = "\\d\\dKT")
  speed[fMPS] <- as.numeric(str_sub(str_extract(x[fMPS], pattern = "\\d\\dMPS"), 1, 2))
  speed[fKT] <- as.numeric(str_sub(str_extract(x[fKT], pattern = "\\d\\dKT"), 1, 2)) * 0.514444
  speed
}
