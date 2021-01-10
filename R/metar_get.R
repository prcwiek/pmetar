#' Get a current METAR report for an airport.
#'
#' A current METAR weather report is downloaded from the web page of NOAA National Weather Service
#' https://aviationweather.gov/metar/ based on an airport four letters ICAO code, International Civil
#' Aviation Organization, or three letters IATA code, International Air Transport Association.
#'
#' @param airport character; ICAO or an IATA airport code.
#'
#' @return a character vector with a current METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get("EPWA")
#' metar_get("CYUL")
#' metar_get("MAD")
#' metar_get("WAW")
#' metar_get(c("epwa", "Mad", "LEBL"))
#'
metar_get <- function(airport = "EPWA"){
  #options(error = NULL)
  # check if x is a data frame
  if(is.data.frame(airport)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
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
  message("Getting information from Aviation Weather Center www.aviationweather.gov/metar")
  link <- paste0("https://aviationweather.gov/metar/data?ids=",
                 airport,
                 "&format=raw&date=0&hours=0")
  tryCatch(
    expr = {
      myfile <- RCurl::getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
    },
    error = function(e){
      stop("pmetar package error: cannot connect to the server!", call. = FALSE)
    }
  )
  metar <- stringr::str_extract(myfile, pattern = "<code>[:print:]+</code>")
  metar <- stringr::str_replace(metar, "<code>", "")
  metar <- stringr::str_replace(metar, "</code>", "")
  metar[is.na(metar)] <- "No METAR found!"
  message("Don't use for flight planning or navigation!")
  metar
}
