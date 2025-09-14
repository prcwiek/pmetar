#' Get a current METAR report for an airport.
#'
#' A current METAR weather report is downloaded from the web page of NOAA National Weather Service
#' https://aviationweather.gov/data/metar/ based on an airport four letters ICAO code, International Civil
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
#'
metar_get <- function(airport = "EPWA"){
  # check if x is a data frame
  if(is.data.frame(airport)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }

  # check if x is a character with length 1
  if(length(airport) > 1){
    stop("pmetar package error: Only one airport at once!", call. = FALSE)
  }

  # check whether airpot consist of spaces
  if(stringr::str_detect(airport, pattern = "\\s")) {
    stop("pmetar package error: Airport code contains blank(s)!", call. = FALSE)
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
  fT <- stringr::str_detect(airport, pattern = "^[0-9A-Z]{4}$")
  # get METAR report for each element
  message("Getting information from Aviation Weather Center aviationweather.gov/data/metar/")

  link <- paste0("https://aviationweather.gov/api/data/metar?ids=",
                 airport,
                 "&format=raw&hours=0")

  answer_POST <- function(p) {
    req_link <- httr2::request(p)
    req_link <- httr2::req_timeout(req_link, 20)    
    tryCatch(
      httr2::req_perform(req_link),
      error = function(e) {
        stop("httr2_failure: Error during request performing!", call. = FALSE)
      },
      warning = function(w) conditionMessage(w)
    )
  }

  # Check internet connection
  if(!curl::has_internet()) {
    message("No internet connection!")
    return(invisible(NULL))
  }

  resp_link <- answer_POST(link)

  # Case for the status '204 No Content'
  if (resp_link$status_code == 204) {
    message(paste0(resp_link$status_code, " ", httr2::resp_status_desc(resp_link)))
    return("No METAR found!")
  }
  
  # Check if status > 200
  if(resp_link$status_code > 200) {
    message(paste0(resp_link$status_code, " ", httr2::resp_status_desc(resp_link)))
    return(invisible(NULL))
  }

  metar <- httr2::resp_body_string(resp_link)
  metar <- stringr::str_replace(metar, "\n", "")
  metar[is.na(metar)] <- "No METAR found!"
  if (metar == "") {
    metar <- "No METAR found!"
  }
  message("Don't use for flight planning or navigation!")
  metar
}
