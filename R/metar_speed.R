#' Extract wind speed
#'
#' Function extract a wind speed value from METAR weather report.
#'
#' @param x Input character vector.
#' @param metric Selection between the metric system and the imperial system. As default metric = TRUE.
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
metar_speed <- function(x, metric = TRUE){
  if(metric){
    cfm <- 1
    cfi <- 0.5144447
  } else {
    cfm <- 1/0.5144447
    cfi <- 1
  }
  speed <- c(1:length(x))
  speed[c(1:length(x))] <- 0
  fMPS <- str_detect(x, pattern = "\\d{5}(MPS|G\\d{2}MPS)")
  fKT <- str_detect(x, pattern = "\\d{5}(KT|G\\d{2}KT)")
  speed[fMPS] <- as.numeric(str_sub(str_extract(x[fMPS], pattern = "\\d{5}(MPS|G\\d{2}MPS)"), 4, 5)) * cfm
  speed[fKT] <- as.numeric(str_sub(str_extract(x[fKT], pattern = "\\d{5}(KT|G\\d{2}KT)"), 4, 5)) * cfi
  speed
}
