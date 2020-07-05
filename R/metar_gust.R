#' Extract gust wind speed.
#'
#' Function extracts a gust speed from METAR weather report.
#'
#' @param x Input character vector
#' @param metric For the default metric = TRUE a returned gust wind speed is in m/s. If it's FALSE, in knots.
#'
#' @return A numeric vector. A gust speed.
#'
#' @export
#'
#' @examples
#' metar_gust("201807141200 METAR EPWA 141200Z 30011G22KT 270V340 9999 -SHRA SCT007 BKN015CB 18/17 Q1011 RESHRA TEMPO BKN007")
#' metar_gust("CYUL 101900Z 27015G25KT 15SM DRSN SCT028 BKN090 OVC110 M04/M10 A2973 RMK")
#' metar_gust("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_gust <- function(x, metric = TRUE) {
  # define conversion coefficients
  if(metric){
    cfm <- 1
    cfi <- 0.5144447
  } else {
    cfm <- 1/0.5144447
    cfi <- 1
  }
  out <- c(1:length(x))
  out[1:length(x)] <- 0
  fKT <- stringr::str_detect(x, pattern = "G\\d\\dKT")
  out[fKT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fKT], pattern = "G\\d+KT"), 2, 3)) * cfi
  fMPS <- stringr::str_detect(x, pattern = "G\\d\\dMPS")
  out[fMPS] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fMPS], pattern = "G\\d\\dMPS"), 2, 3)) * cfm
  out
}
