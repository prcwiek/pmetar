#' Extract approximated airport location.
#'
#' Function extract approximated airport lattitude, longitude and elevation for a given airport code.
#'
#' @param x Input character vector
#'
#' @return A tible with three columns longitude, latitude, elevation in meters and one row
#' or with four columns longitude, latitude, elevation in meters.
#'
#' @export
#'
#' @examples
#' metar_location("EPWA")
#' metar_location("CYUL")
#' metar_location("LEMD")
#'
metar_location <- function(x, ICAO = FALSE) {
  x <- x[1]
  if(nchar(x[1]) == 0){
    cat("Airport not specified!\n")
  } else if (sum(str_count(mst, pattern = x)) < 1) {
    cat(paste("Airport ", x, " not found!\n", sep = ""))
  } else {
    m_t <- str_extract(mst, pattern = paste(x, "(?:[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+)[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
    m_t <- m_t[!is.na(m_t)]
    lat <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:N|S)")
    if(str_sub(lat, nchar(lat), nchar(lat)) == "N"){
      mlat <- 1
    } else {
      mlat  <- -1
    }
    lat <- str_sub(lat, 1, nchar(lat) - 1)
    lat <- str_split(lat, " ")
    lat <- (as.numeric(lat[[1]][1]) + as.numeric(lat[[1]][2])/60) * mlat
    #lat <- as.numeric(str_replace(lat, " ", "."))
    lon <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:E|W)")
    if(str_sub(lon, nchar(lon), nchar(lon)) == "E"){
      mlon <- 1
    } else {
      mlon  <- -1
    }
    lon <- str_sub(lon, 1, nchar(lon) - 1)
    lon <- str_split(lon, " ")
    lon <- (as.numeric(lon[[1]][1]) + as.numeric(lon[[1]][2])/60) * mlon
    #lon <- as.numeric(str_replace(lon, " ", "."))
    ele <- as.numeric(str_extract(m_t, pattern = "[\\d]+$"))
    if(ICAO){
      data_frame(name = x, longitude = lon, latitude = lat, elevation = ele)
    } else{
      data_frame(longitude = lon, latitude = lat, elevation = ele)
    }
  }
}
