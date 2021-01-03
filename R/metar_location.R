#' Get approximated airport location.
#'
#' Find approximated latitude, longitude and elevation of an airport according to
#' IATA, International Air Transport Association, or
#' ICAO, International Civil Aviation Organization, airport code. Two source of
#' information about airports are used. First the function search in the list of
#' airports available at
#' [https://ourairports.com/data/airports.csv](https://ourairports.com/data/airports.csv)
#' created by David Megginson.
#' If an airport cannot be found there, the second list of airports is searched, from
#' [https://www.aviationweather.gov/docs/metar/stations.txt](https://www.aviationweather.gov/docs/metar/stations.txt)
#' prepared by Greg Thompson from \cr
#' National Weather Service NCAR/RAP.
#'
#' @param x character vector; an airport ICAO four letters code or an IATA three letters code.
#'
#' @return a tibble with columns with an airport information as below:
#' \itemize{
#' \item ICAO code
#' \item IATA Code
#' \item Airport name
#' \item Longitude, in degress
#' \item Latitude, in degress
#' \item Elevation, above see elevel in meters
#' \item Source of information
#' }
#'
#' @export
#'
#' @examples
#' metar_location("EPWA")
#' metar_location("CYUL")
#' metar_location("LEMD")
#' metar_location("NCRK")
#' metar_location("WAW")
#' metar_location("FRA")
#'
metar_location <- function(x = "EPWA") {

  # Additional function
  mystr_extract <- function(p){
    m_t <- stringr::str_extract(mst, pattern = paste(p, "(?:[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+)[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
    m_t <- m_t[!is.na(m_t)]
    if(length(m_t) > 1){
      m_t <- m_t[1]
    }
    # extract latitude
    lat <- stringr::str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:N|S)")
    if(stringr::str_sub(lat, nchar(lat), nchar(lat)) == "N"){
      mlat <- 1
    } else {
      mlat  <- -1
    }
    lat <- stringr::str_sub(lat, 1, nchar(lat) - 1)
    lat <- stringr::str_split(lat, " ")
    lat <- (as.numeric(lat[[1]][1]) + as.numeric(lat[[1]][2])/60) * mlat
    # extract longitude
    lon <- stringr::str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:E|W)")
    if(stringr::str_sub(lon, nchar(lon), nchar(lon)) == "E"){
      mlon <- 1
    } else {
      mlon  <- -1
    }
    lon <- stringr::str_sub(lon, 1, nchar(lon) - 1)
    lon <- stringr::str_split(lon, " ")
    lon <- (as.numeric(lon[[1]][1]) + as.numeric(lon[[1]][2])/60) * mlon
    # extract elevation in meters
    ele <- as.numeric(stringr::str_extract(m_t, pattern = "[\\d]+$"))
    # extract airport name
    m_t <- stringr::str_extract(mst, pattern = paste("^(.*?)", p, sep = ""))
    m_t <- m_t[!is.na(m_t)]
    apname <- stringr::str_extract(m_t, pattern = paste("^(.*?)", p, sep = ""))
    apname <- stringr::str_trim(apname)
    apname <- stringr::str_split(apname, pattern = " ", simplify = TRUE)
    apname <- apname[1,1:ncol(apname) - 1]
    apname <- stringr::str_c(apname, collapse = " ")
    apname <- stringr::str_trim(apname)
    list(apname, lat, lon, ele)
  }

  message("Getting airport informaiton from the file downloaded from")
  message("https://ourairports.com/data/airports.csv")
  message("created by David Megginson")
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # all characters to upper cases
  x <- stringr::str_to_upper(x)
  # find IATA codes
  fT <- stringr::str_detect(x, pattern = "^[A-Z]{3}$")
  # convert IATA codes to ICAO codes
  x[fT] <- ourairports$ident[match(x[fT], ourairports$iata_code)]
  nmatched <- match(x, ourairports$ident)
  if (sum(stringr::str_count(x, pattern = "^[A-Za-z]{4}$")) >= 1) {
    outlocation <- dplyr::tibble(
      ICAO_Code = x,
      IATA_Code = ourairports$iata_code[nmatched],
      Airport_Name = ourairports$name[nmatched],
      Longitude = round(ourairports$longitude_deg[nmatched], 5),
      Latitude = round(ourairports$latitude_deg[nmatched], 5),
      Elevation = round(ourairports$elevation_m[nmatched], 4),
      Source = "http://ourairports.com/data/airports.csv"
    )
  }
  # try to use the other source of airports locations
  # IATA code is available
  if((sum(stats::complete.cases(outlocation$ICAO_Code)) == nrow(outlocation)) &
     (sum(stats::complete.cases(outlocation$IATA_Code)) != nrow(outlocation))){
    message("Getting airport informaiton from the file downloaded from")
    message("www.aviationweather.gov/docs/metar/stations.txt")
    message("prepared by Greg Thompson National Weather Service NCAR/RAP")
    m_l <- c(1:length(x))
    m_l[1:length(x)] <- ""
    nmissing <- which(is.na(outlocation$IATA_Code) & !is.na(outlocation$ICAO_Code))
    m_l <- sapply(x, mystr_extract)
    outlocation$IATA_Code[nmissing] <- "Not found!"
    outlocation$Airport_Name[nmissing] <- unlist(m_l[1, nmissing])
    outlocation$Longitude[nmissing] <- round(unlist(m_l[3, nmissing]), 2)
    outlocation$Latitude[nmissing] <- round(unlist(m_l[2, nmissing]), 2)
    outlocation$Elevation[nmissing] <- round(unlist(m_l[4, nmissing]), 0)
    outlocation$Source[nmissing] <- "www.aviationweather.gov/docs/metar/stations.txt"
  }
  outlocation
}
