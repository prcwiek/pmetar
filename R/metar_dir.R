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
  if(str_detect(x, pattern = one_or_more(DGT) %R% "G" %R% one_or_more(DGT) %R% "KT")){
    dirw <- str_extract(x, pattern = one_or_more(DGT) %R% "G" %R% one_or_more(DGT) %R% "KT")
    as.numeric(str_sub(dirw, 1, 3))
  } else if(str_detect(x, pattern = "VRB" %R% one_or_more(DGT) %R% "KT")){
    dirw <- NA
    dirw
  } else {
    dirw <- str_extract(x, pattern = one_or_more(DGT) %R% "KT")
    as.numeric(str_sub(dirw, 1, 3))
  }
}
