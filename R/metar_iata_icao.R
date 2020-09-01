#' Convert between IATA (International Air Transport Association) airport code to
#' ICAO (International Civil Aviation Organization) airport code or vice versa.
#'
#' @param code character vector; an airport ICAO four letters code or an IATA three letters code.
#'
#' @return a character vector with an IATA code an ICAO input code or an ICAO code an IATA input code.
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
metar_iata_icao <- function(code = "WAW") {
  message("Getting airport informaiton from the file downloaded from")
  message("http://ourairports.com/data/airports.csv")
  # check if x is a data frame
  if(is.data.frame(code)){
    stop("ERROR: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(code))
  out[1:length(code)] <- "Incorrect ICAO or IATA airport code!"
  # all characters to upper cases
  x <- stringr::str_to_upper(code)
  # convert ICAO to IATA
  fT <- stringr::str_detect(x, pattern = "^[A-Z]{4}$")
  out[fT] <- ourairports$iata_code[match(x[fT], ourairports$ident)]
  # convert IATA to ICAO
  fT <- stringr::str_detect(x, pattern = "^[A-Z]{3}$")
  out[fT] <- ourairports$ident[match(x[fT], ourairports$iata_code)]
  out[is.na(out)] <- "Incorrect ICAO or IATA airport code!"
  out[out == ""] <- "Not found!"
  out
}
