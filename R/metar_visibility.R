#' Extract visibility.
#'
#' Function extracts visibility information from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param metric For the default metric = TRUE returned distances are in meters. If it's FALSE, in miles.
#'
#' @return A numeric vector with visibility information, in meters or miles.
#'
#' @export
#'
#' @examples
#' metar_visibility("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_visibility("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_visibility("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#' metar_visibility("KBLV 011657Z AUTO 25015G30KT 210V290 3/8SM R32L/1000FT FG BKN005 01/M01 A2984 RMK A02 SLP03")
#'
metar_visibility <- function(x, metric = TRUE) {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # define conversion coefficients
  if(metric){
    cfm <- 1
    cfi <- 1609.344
  } else {
    cfm <- 1/1609.344
    cfi_height <- 1
    cfi <- 1
  }
  out <- c(1:length(x))
  out[1:length(x)] <- NA
  # cases like 1 3/4SM
  fT <- stringr::str_detect(x, pattern = "\\s\\d\\s\\d\\/\\dSM\\s")
  out[fT] <- (as.numeric(stringr::str_extract(stringr::str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\s\\d\\s")) +
                   as.numeric(stringr::str_sub(stringr::str_extract(stringr::str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\d\\/"), 1, 1)) /
                   as.numeric(stringr::str_sub(stringr::str_extract(stringr::str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\/\\d"), -1, -1))) * cfi
  # cases like 3/4SM
  fT2 <- stringr::str_detect(x, pattern = "\\s\\d\\/\\dSM\\s")
  fT <- !(fT & fT2)
  out[fT] <- (as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\s\\d\\/\\dSM\\s"), 2, 2)) /
                   as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\s\\d\\/\\dSM\\s"), 4, 4))) * cfi
  fT <- stringr::str_detect(x, pattern = "\\s\\d{4}\\s")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\s[\\d]{4}\\s"), 2, 5)) * cfm
  fT <- stringr::str_detect(x, pattern = "\\s[\\d]+SM\\s")
  out[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\s[\\d]+SM\\s"), 1, -4)) * cfi
  # CAVOK - Ceiling And Visibility OK, indicating no cloud below 5,000 ft (1,500 m) or
  # the highest minimum sector altitude and no cumulonimbus or towering cumulus at any level,
  # a visibility of 10 km (6 mi) or more and no significant weather change
  fT <- stringr::str_detect(x, pattern = "CAVOK")
  out[fT] <- "Ceiling And Visibility OK"
  out
}
