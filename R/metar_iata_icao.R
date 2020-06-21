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
metar_iata_icao <- function(x) {
  cat("Getting airport informaiton from the file downloaded from\n")
  cat("http://ourairports.com/data/airports.csv\n")

  if(str_detect(x, pattern = "^[A-Za-z]{4}$")){
    out <- ourairports %>%
      filter(ident == x) %>%
      select(iata_code)
  } else if(str_detect(x, pattern = "^[A-Za-z]{3}$")){
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
