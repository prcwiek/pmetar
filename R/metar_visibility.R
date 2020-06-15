#' Extract visibility.
#'
#' Function extract a visibility value from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A numeric vector. A visibility in meters.
#'
#' @export
#'
#' @examples
#' metar_visiblity("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_visiblity("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_visiblity("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_visibility <- function(x) {
  outvis <- c(1:length(x))
  outvis[1:length(x)] <- NA
  fT <- str_detect(x, pattern = "\\s[\\d]+\\s")
  outvis[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "\\s[\\d]+\\s"), 2, 5))
  fT <- str_detect(x, pattern = "\\s[\\d]+SM\\s")
  outvis[fT] <- as.numeric(str_sub(str_extract(x[fT], pattern = "\\s[\\d]+SM\\s"), 1, -4)) * 1609.344
  # CAVOK - Ceiling And Visibility OK, indicating no cloud below 5,000 ft (1,500 m) or
  # the highest minimum sector altitude and no cumulonimbus or towering cumulus at any level,
  # a visibility of 10 km (6 mi) or more and no significant weather change
  fT <- str_detect(x, pattern = "CAVOK")
  outvis[fT] <- "Ceiling And Visibility OK"
  # cases like 1 3/4SM
  fT <- str_detect(x, pattern = "\\s\\d\\s\\d\\/\\dSM\\s")
  outvis[fT] <- (as.numeric(str_extract(str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\s\\d\\s")) +
    as.numeric(str_sub(str_extract(str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\d\\/"), 1, 1)) /
    as.numeric(str_sub(str_extract(str_extract(x[fT], pattern = "\\s\\d\\s\\d\\/\\dSM\\s"), pattern = "\\/\\d"), -1, -1))) * 1609.344
  # cases like 3/4SM
  fT <- str_detect(x, pattern = "KT\\s\\d\\/\\dSM\\s")
  outvis[fT] <- (as.numeric(str_sub(str_extract(x[fT], pattern = "KT\\s\\d\\/\\dSM\\s"), 4, 4)) /
                 as.numeric(str_sub(str_extract(x[fT], pattern = "KT\\s\\d\\/\\dSM\\s"), 6, 6))) * 1609.344
  outvis
}

