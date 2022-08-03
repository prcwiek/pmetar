#' Get wind speed
#'
#' Extract a wind speed value from METAR weather report.
#'
#' @param x character vector; METAR weather report or reports.
#' @param metric logical; the default value is TRUE and a returned wind speed is in m/s;
#' if it's FALSE then in knots.
#' @param check logical; the default value is FALSE, if METAR report fails the syntax
#' check, NA value will be returned. If FALSE, zero values will be returned for 
#' METAR reports with incorrect syntax. 
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
metar_speed <- function(x, metric = TRUE, check = FALSE){
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
  out <- rep(NA, length(x))
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  # check syntax
  if (check) {
    icorrect <- metar_is_correct(x)
  } else {
    icorrect <- rep(TRUE, length(x))
  }
  x <- x[which(icorrect)]
  outx <- c(1:length(x))
  outx[c(1:length(x))] <- NA
  fMPS <- stringr::str_detect(x, pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS)")
  fKT <- stringr::str_detect(x, pattern = "(\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS)")
  outx[fMPS] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fMPS],
                                                                pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS)")
                                            , 4, 5)) * cfm
  outx[fKT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fKT],
                                                               pattern = "(\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS)")
                                           , 4, 5)) * cfi
  fMPS <- stringr::str_detect(x, pattern = "\\d{3}P49MPS")
  fKT <- stringr::str_detect(x, pattern = "\\d{3}P99KT")
  if (metric) {
    outx[fMPS] <- 50
    outx[fKT] <- 50
  } else {
    outx[fMPS] <- 100
    outx[fKT] <- 100
  }

  out[which(icorrect)] <- outx
  out
}
