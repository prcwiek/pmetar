#' Get a current METAR report for an airport.
#'
#' Function gets metar report from a web page.\cr
#' \cr
#' If only airport is passed as an argument, a current METAR weather report is donwloaded from\cr
#' the web page https://aviationweather.gov/metar/\cr
#' \cr
#'
#' @param airport Input character vector with an ICAO airport code
#'
#' @return A character vector with a current METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get("EPWA")
#' metar_get("CYUL")
#' metar_get("LEMD")
#' metar_get("WAW")
#'
metar_get <- function(airport = "EPWA"){
  airport <- stringr::str_to_upper(airport)
  if(stringr::str_detect(airport, pattern = "^[A-Z]{3}$")) {
    airport <- metar_iata_icao(airport)
  }
  if(stringr::str_detect(airport, pattern = "^[A-Z]{4}$")) {
    cat("Getting information from Aviation Weather Center www.aviationweather.gov/metar\n")
    link <- paste0("https://aviationweather.gov/metar/data?ids=",
                   airport,
                   "&format=raw&date=0&hours=0")
    myfile <- RCurl::getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
    metar <- stringr::str_extract(myfile, pattern = "<code>[:print:]+</code>")
    metar <- stringr::str_replace(metar, "<code>", "")
    metar <- stringr::str_replace(metar, "</code>", "")
    if(stringr::str_detect(metar, pattern = "No METAR found")){
      cat(paste("No METAR found for ", airport, "!\n", sep = ""))
    } else stringr::str_split(metar, "<br", simplify = TRUE)[1]
  } else {
    cat("Incorrect ICAO airport code!\n")
  }
}
