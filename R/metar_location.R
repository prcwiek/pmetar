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


  if (sum(str_count(x, pattern = "^[A-Za-z]{4}$")) >= 1) {
    outlocation <- tibble(
      ICAO.code = x,
      IATA.code = ourairports$iata_code[nmatched],
      airport.name = ourairports$name[nmatched],
      longitude = ourairports$longitude_deg[nmatched],
      latitude = ourairports$latitude_deg[nmatched],
      elevation = ourairports$elevation_m[nmatched],
      Source = "http://ourairports.com/data/airports.csv"
    )
    return(outlocation)
  } else{
    outlocation <- tibble(
      ICAO.code = ourairports$ident[nmatched],
      IATA.code = x,
      airport.name = ourairports$name[nmatched],
      longitude = ourairports$longitude_deg[nmatched],
      latitude = ourairports$latitude_deg[nmatched],
      elevation = ourairports$elevation_m[nmatched],
      Source = "http://ourairports.com/data/airports.csv"
    )
    return(outlocation)
  }

  # try to use the other source of airports locations
  # IATA code is available
  if((sum(complete.cases(outlocation$ICAO.code)) == nrow(outlocation)) &
      (sum(complete.cases(outlocation$IATA.code)) != nrow(outlocation))){
    cat("Getting airport informaiton from the file downloaded from\n")
    cat("www.aviationweather.gov/docs/metar/stations.txt\n")
    cat("prepared by Greg Thompson NCAR/RAP\n")
    mystr_extract <- function(p){
      m_t <- str_extract(mst, pattern = paste(p, "(?:[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+)[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
      m_t <- m_t[!is.na(m_t)]
      if(length(m_t) > 1){
        m_t <- m_t[1]
      }
      # extract latitude
      lat <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:N|S)")
      if(str_sub(lat, nchar(lat), nchar(lat)) == "N"){
        mlat <- 1
      } else {
        mlat  <- -1
      }
      lat <- str_sub(lat, 1, nchar(lat) - 1)
      lat <- str_split(lat, " ")
      lat <- (as.numeric(lat[[1]][1]) + as.numeric(lat[[1]][2])/60) * mlat
      # extract longitude
      lon <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:E|W)")
      if(str_sub(lon, nchar(lon), nchar(lon)) == "E"){
        mlon <- 1
      } else {
        mlon  <- -1
      }
      lon <- str_sub(lon, 1, nchar(lon) - 1)
      lon <- str_split(lon, " ")
      lon <- (as.numeric(lon[[1]][1]) + as.numeric(lon[[1]][2])/60) * mlon
      # extract elevation in meters
      ele <- as.numeric(str_extract(m_t, pattern = "[\\d]+$"))
      # extract airport name
      m_t <- str_extract(mst, pattern = paste("^(.*?)", p, sep = ""))
      m_t <- m_t[!is.na(m_t)]
      apname <- str_extract(m_t, pattern = paste("^(.*?)", p, sep = ""))
      apname <- str_trim(apname)
      apname <- str_split(apname, pattern = " ", simplify = TRUE)
      apname <- apname[1,1:ncol(apname) - 1]
      apname <- str_c(apname, collapse = " ")
      apname <- str_trim(apname)
      list(apname, lat, lon, ele)
    }

    m_l <- c(1:length(x))
    m_l[1:length(x)] <- ""
    nmissing <- which(is.na(outlocation$IATA.code) & !is.na(outlocation$ICAO.code))
    m_l <- sapply(x, mystr_extract)
    outlocation$IATA.code[nmissing] <- "not found"
    outlocation$airport.name[nmissing] <- unlist(m_l[1, nmissing])
    outlocation$longitude[nmissing] <- unlist(m_l[3, nmissing])
    outlocation$latitude[nmissing] <- unlist(m_l[2, nmissing])
    outlocation$elevation[nmissing] <- unlist(m_l[4, nmissing])
    outlocation$Source[nmissing] <- "www.aviationweather.gov/docs/metar/stations.txt"
  }

  outlocation

}
