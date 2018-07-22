#' Extract approximated airport location.
#'
#' Function extract approximated lattitude, longitude and elevation for a given airport code.
#'
#' @param x Input character vector, an airport ICAO four letters code or IATA three letters code
#'
#' @return A tibble with columns which consists of airport ICAO code, IATA code airport name, longitude, latitude,
#'  elevation in meters, source of information
#'
#' @export
#'
#' @examples
#' metar_location("EPWA")
#' metar_location("CYUL")
#' metar_location("LEMD")
#' metar_loaction("WAW")
#' metar_location("FRA")
#'
metar_location <- function(x) {
  cat("Getting airport informaiton from the file downloaded from\n")
  cat("http://ourairports.com/data/airports.csv\n")

  if(sum(str_count(x, pattern = "^[A-Za-z]{4}$")) >= 1){
    nmatched <- match(x, ourairports$ident)
  } else if(sum(str_count(x, pattern = "^[A-Za-z]{3}$")) >= 1){
    nmatched <- match(x, ourairports$iata_code)
  } else {
    stop("All ICAO or/and IATA airport code are incorrect!\n", call. = FALSE)
  }

  data_frame(ICAO.code = ourairports$ident[nmatched],
             IATA.code = ourairports$iata_code[nmatched],
             airport.name = ourairports$name[nmatched],
             longitude = ourairports$longitude_deg[nmatched],
             latitude = ourairports$latitude_deg[nmatched],
             elevation = ourairports$elevation_m[nmatched],
             Source = "http://ourairports.com/data/airports.csv")
}
