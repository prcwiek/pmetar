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
  fMPS <- str_detect(x, pattern = "[\\d]+MPS")
  fKT <- str_detect(x, pattern = "[\\d]+KT")
  speed <- c(1:length(x))
  speed[fMPS] <- as.numeric(str_sub(str_extract(x[fMPS], pattern = "[\\d]+MPS"), 1, 2))
  speed[fKT] <- as.numeric(str_sub(str_extract(x[fKT], pattern = "[\\d]+KT"), 1, 2)) * 0.514444
  speed
  # if(str_detect(x, pattern = "[\\d]+MPS")) {
  #   speed <- str_extract(x, pattern = "[\\d]+MPS")
  #   as.numeric(str_sub(speed, 1, 2))
  # } else if(str_detect(x, pattern = "[\\d]+KT")) {
  #   speed <- str_extract(x, pattern = "[\\d]+KT")
  #   as.numeric(str_sub(speed, 1, 2)) * 0.514444
  # }
}
