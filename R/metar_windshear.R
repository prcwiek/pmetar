#' Get wind shear information.
#'
#' Function extracts information about wind shear from METAR weather report.
#'
#' @param x character vector; METAR weather report or reports.
#' @param metric For the default metric = TRUE a returned wind speed is in m/s. If it's FALSE, in knots.
#'
#' @return A character vector with information about wind shear.
#'
#' @export
#'
#' @examples
#' metar_windshear("METAR VHHH 180800Z 12009KT 060V150 FEW010 SCT045 22/18 Q1012 WS R07R")
#' metar_windshear("CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D M05/M08 A2992 WS RWY36")
#' metar_windshear("KPIT 091730Z 091818 22020KT 3SM -SHRA BKN020 WS015/30045KT",
#'  metric = FALSE)
#'
metar_windshear <- function(x, metric = TRUE) {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # define conversion coefficients
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
  out <- c(1:length(x))
  out[c(1:length(x))] <- NA
  fT <- stringr::str_detect(x, pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)")
  out[fT] <- paste0("Wind shear layer ",
                         as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "WS\\d{3}"), 3, 5)) * 100 * cfi,
                         theight,
                         round(metar_speed(stringr::str_sub(stringr::str_extract(x[fT], pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)"), 7, -1), metric), 1),
                         tspeed,
                         metar_dir(stringr::str_sub(stringr::str_extract(x[fT], pattern = "WS\\d{3}\\/\\d{5}(KT|MPS)"), 7, -1)),
                         " degrees.")
  fT <- stringr::str_detect(x, pattern = "WS (R\\d|RWY\\d)")
  out[fT] <- paste0("Wind shear runway ", stringr::str_sub(stringr::str_extract(x[fT], pattern = "WS (R|RWY)\\d+\\w+"), 4, -1))
  fT <- stringr::str_detect(x, pattern = "WS ALL RWY")
  out[fT] <- "Wind shear all runways"
  out
}



