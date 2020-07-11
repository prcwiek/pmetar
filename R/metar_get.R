#' Get a current METAR report for an airport.
#'
#' Function gets metar report from a web page.\cr
#' \cr
#' If only airport is passed as an argument, a current METAR weather report is donwloaded from\cr
#' the web page https://aviationweather.gov/metar/\cr
#' \cr
#'
#' @param x character; ICAO or an IATA airport code.
#'
#' @return A character vector with a current METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get("EPWA")
#' metar_get("CYUL")
#' metar_get("MAD")
#' metar_get("WAW")
#'
metar_get <- function(x = "EPWA"){
  # convert data frame
  if (is.data.frame(x)) {
    airport <- x[,1]
  } else {
    airport <- x
  }
  out <- c(1:length(airport))
  out[1:length(airport)] <- NA
  # all characters to upper cases
  airport <- stringr::str_to_upper(airport)
  # find IATA codes
  fT <- stringr::str_detect(airport, pattern = "^[A-Z]{3}$")
  # convert IATA codes to ICAO codes
  airport[fT] <- metar_iata_icao(airport[fT])
  # find IACO codes
  fT <- stringr::str_detect(airport, pattern = "^[A-Z]{4}$")
  # get METAR report for each element
  cat("Getting information from Aviation Weather Center www.aviationweather.gov/metar\n")
  link <- paste0("https://aviationweather.gov/metar/data?ids=",
                 airport,
                 "&format=raw&date=0&hours=0")
  myfile <- RCurl::getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
  metar <- stringr::str_extract(myfile, pattern = "<code>[:print:]+</code>")
  metar <- stringr::str_replace(metar, "<code>", "")
  metar <- stringr::str_replace(metar, "</code>", "")
  metar[is.na(metar)] <- "No METAR found!"
  metar
}
