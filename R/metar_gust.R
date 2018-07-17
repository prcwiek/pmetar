#' Function extract a gust speed from METAR weather report.
#'
#' @param x Input character vector
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
metar_gust <- function(x) {
  gw <- c(1:length(x))
  gw[1:length(x)] <- NA
  fGW <- str_detect(x, pattern = "\\d\\dG")
  gw[fGW] <- as.numeric(str_sub(str_extract(x[fGW], pattern = "\\d\\dG"), 1, 2))
  fKT <- str_detect(x, pattern = "\\d\\dKT")
  gw[fKT] <- gw[fKT] * 0.514444
  gw
}
