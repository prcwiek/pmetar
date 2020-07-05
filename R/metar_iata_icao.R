#' Convert IATA, International Air Transport Association, airport code to\cr
#' ICAO, International Civil Aviation Organization,  airport code and vice versa.
#'
#' @param x character; an airport ICAO four letters code or an IATA three letters code.
#'
#' @return A character vector with an IATA code an ICAO input code or an ICAO code an IATA input code.
#'
#' @importFrom magrittr %>%
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
      dplyr::filter(ident == x) %>%
      dplyr::select(iata_code)
  } else if(stringr::str_detect(x, pattern = "^[A-Z]{3}$")){
    out <- ourairports %>%
      dplyr::filter(iata_code == x) %>%
      dplyr::select(ident)
  } else {
    stop("Incorrect ICAO or IATA airport code!\n", call. = FALSE)
  }
  if(nrow(out) > 0){
    as.character(out)
  } else{
    cat("Airport cannot be found!\n")
  }
}
