#' Get wind speed
#'
#' Extract a wind speed value from METAR weather report.
#'
#' @param x character vector; METAR weather report or reports.
#' @param metric logical; the default value is TRUE and a returned wind speed is in m/s;
#' if it's FALSE then in knots.
#'
#' @return a numeric vector. A wind speed in m/s or in knots.
#'
#' @export
#'
#' @examples
#' metar_speed("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_speed("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180", metric = FALSE)
#' metar_speed("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#' metar_speed("EPKK 141730Z VRB01KT CAVOK 21/16 Q1028")
#'
metar_speed <- function(x, metric = TRUE){
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # define conversion coefficients
  if(metric){
    cfm <- 1
    cfi <- 0.5144447
  } else {
    cfm <- 1/0.5144447
    cfi <- 1
  }
  out <- c(1:length(x))
  out[c(1:length(x))] <- 0
  fMPS <- stringr::str_detect(x, pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS)")
  fKT <- stringr::str_detect(x, pattern = "(\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT)")
  out[fMPS] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fMPS],
                                                                pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS)"), 4, 5)) * cfm
  out[fKT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fKT],
                                                               pattern = "(\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT)"), 4, 5)) * cfi
  out
}
