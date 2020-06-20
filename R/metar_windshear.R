#' Extract wind shear information
#'
#' Function extracts a wind speed value from METAR weather report.
#'
#' @param x Input character vector.
#' @param metric Selection between the metric system and the imperial system. As default metric = TRUE.
#'
#' @return A character vector with information about wind shear.
#'
#' @export
#'
#' @examples
#' metar_windshear("202003180800 METAR VHHH 180800Z 12009KT 060V150 9999 FEW010 SCT045 22/18 Q1012 WS R07R NOSIG")
#' metar_windshear("CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN BLSN BKN008 OVC040 M05/M08 A2992 REFZRA WS RWY36 RMK SF5NS3 SLP134")
#' metar_windshear("KPIT 091730Z 091818 22020KT 3SM -SHRA BKN020 WS015/30045KT", metric = FALSE)
#'
metar_windshear <- function(x, metric = TRUE) {
  if(metric){
    cfm <- 1
    cfi <- 0.3048
    theight <- " m, "
    tspeed <- " m/s "
  } else {
    cfm <- 1/0.3048
    cfi <- 1
    theight <- " ft, "
    tspeed <- " kt "
  }
  outwsh <- c(1:length(x))
  outwsh[c(1:length(x))] <- NA
  fWSH <- str_detect(x, pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)")
  outwsh[fWSH] <- paste0("Wind shear layer ",
                         as.numeric(str_sub(str_extract(x[fWSH], pattern = "WS\\d{3}"), 3, 5)) * 100 * cfi,
                         theight,
                         round(metar_speed(str_sub(str_extract(x[fWSH], pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)"), 7, -1), metric), 1),
                         tspeed,
                         metar_dir(str_sub(str_extract(x[fWSH], pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)"), 7, -1)),
                         " degrees.")
  fWSH <- str_detect(x, pattern = "WS R")
  outwsh[fWSH] <- paste0("Wind shear runway ", str_sub(str_extract(x[fWSH], pattern = "WS (R|RWY)\\d+\\w+"), 4, -1))
  fWSH <- str_detect(x, pattern = "WS ALL RWY")
  outwsh[fWSH] <- "Wind shear all runways"
  outwsh
}


