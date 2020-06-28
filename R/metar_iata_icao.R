#' Convert IATA airport code to ICAO airport code and vice versa.
#'
#' @param x Input character vector, an airport ICAO four letters code or IATA three letters code
#'
#' @param x Input character vector.
#'
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' metar_iata_icao("EPWA")
#' metar_iata_icao("CYUL")
#' metar_iata_icao("LEMD")
#' metar_iata_icao("WAW")
#' metar_iata_icao("FRA")
#' metar_iata_icao("KRK")
#'
metar_iata_icao <- function(x = "WAW") {
  cat("Getting airport informaiton from the file downloaded from\n")
  cat("http://ourairports.com/data/airports.csv\n")
  x <- stringr::str_to_upper(x)
  if(stringr::str_detect(x, pattern = "^[A-Z]{4}$")){
    out <- ourairports %>%
      filter(ident == x) %>%
      select(iata_code)
  } else if(stringr::str_detect(x, pattern = "^[A-Z]{3}$")){
    out <- ourairports %>%
      filter(iata_code == x) %>%
      select(ident)
  } else {
    stop("Incorrect ICAO or IATA airport code!\n", call. = FALSE)
  }

  if(nrow(out) > 0){
    as.character(out)
  } else{
    cat("Airport cannot be found!\n")
  }
}
