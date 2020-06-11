#' Extract temperature.
#'
#' Function extract a temperature value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A temperature in degrees Celsius.
#'
#' @export
#'
#' @examples
#' metar_temp("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_temp("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_temp("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_temp <- function(x){
  outtemp <- c(1:length(x))
  outtemp[1:length(x)] <- -273
  fT <- str_detect(x, pattern = "M[\\d]+/")
  outtemp[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "M[\\d]+/"), 2, 3)) * -1
  fT <- str_detect(x, pattern = "\\s[\\d]+/")
  outtemp[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "\\s[\\d]+/"), 2, 3))
  fT <- str_detect(x, pattern = "T[\\d]{4}")
  if(fT){
    if(str_extract(x[fT], pattern = "T[\\d]") == "T0"){
      outtemp[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "T[\\d]{4}"), 3, 5)) / 10.0
    } else {
      outtemp[fT] <- -1.0 * as.numeric(str_sub(str_extract(x[fT], pattern = "T[\\d]{4}"), 3, 5)) / 10.0
    }
  }
  outtemp
}
