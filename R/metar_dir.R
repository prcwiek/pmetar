#' Extract wind direction.
#'
#' Function extract a wind direction value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A wind direction in degrees.
#'
#' @export
#'
#' @examples
#' metar_dir("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dir("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_dir("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_dir <- function(x){
  oldwarn <- getOption("warn")
  options(warn = -1)
  dirw <- c(1:length(x))
  dirw[1:length(x)] <- -1
  fDIR <- str_detect(x, pattern = "([\\d]+G[\\d]+KT|[\\d]+KT|[\\d]+MPS)")
  dirw[fDIR] <- as.numeric(str_sub(str_extract(x[fDIR], pattern = "([\\d]+G[\\d]+KT|[\\d]+KT|[\\d]+MPS)"), 1, 3))
  fDIR <- str_detect(x, pattern = "VRB[\\d]+KT")
  dirw[fDIR] <- NA
  options(warn = oldwarn)
  dirw
}
