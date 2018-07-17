#' Extract approximated airport location.
#'
#' Function extract approximated lattitude, longitude and elevation for a given airport code.
#'
#' @param x Input character vector, an airport ICAO code
#' @param ICAO if TRUE an airport ICAO code is added to an output tibble
#' @param apname if TRUE an airport name is added to an output tibble
#'
#' @return A tibble with columns which consists of airport ICAO code, airport name, longitude, latitude,
#'  elevation in meters, source of information, author of source
#'
#' @export
#'
#' @examples
#' metar_location("EPWA")
#' metar_location("CYUL")
#' metar_location("LEMD")
#'
metar_location <- function(x, ICAO = FALSE, apname = FALSE) {
  x <- x[1]
  if(nchar(x[1]) == 0){
    cat("Airport not specified!\n")
  } else if (sum(str_count(mst, pattern = x)) < 1) {
    cat(paste("Airport ", x, " not found!\n", sep = ""))
  } else {
    cat("Getting airport informaiton from the file downloaded form\n")
    cat("www.aviationweather.gov/docs/metar/stations.txt\n")
    cat("prepared by Greg Thompson NCAR/RAP\n")
    # extract string with information
    m_t <- str_extract(mst, pattern = paste(x, "(?:[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+[\\d]+[\\s]+|\\s\\s...[\\s]+)[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
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
    #create output data frame
    if(ICAO & !apname){
      data_frame(ICAO.code = x, longitude = lon, latitude = lat, elevation = ele,
                 Source = "www.aviationweather.gov/docs/metar/stations.txt",
                 Author.of.Source = "Greg ThompsonNCAR/RAP")
    } else if(apname & !ICAO){
      # extract airport name
      m_t <- str_extract(mst, pattern = paste("^(.*?)", x, sep = ""))
      m_t <- m_t[!is.na(m_t)]
      if(length(m_t) > 1){
        m_t <- m_t[1]
      }
      m_t <- str_trim(m_t)
      m_t <- str_split(m_t, pattern = " ", simplify = TRUE)
      m_t <- m_t[,1:ncol(m_t) - 1]
      m_t <- str_c(m_t, collapse = " ")
      m_t <- str_trim(m_t)
      data_frame(airport.name = m_t, longitude = lon, latitude = lat, elevation = ele,
                 Source = "www.aviationweather.gov/docs/metar/stations.txt",
                 Author.of.Source = "Greg ThompsonNCAR/RAP")
    } else if(ICAO & apname){
      # extract airport name
      m_t <- str_extract(mst, pattern = paste("^(.*?)", x, sep = ""))
      m_t <- m_t[!is.na(m_t)]
      if(length(m_t) > 1){
        m_t <- m_t[1]
      }
      m_t <- str_trim(m_t)
      m_t <- str_split(m_t, pattern = " ", simplify = TRUE)
      m_t <- m_t[,1:ncol(m_t) - 1]
      m_t <- str_c(m_t, collapse = " ")
      m_t <- str_trim(m_t)
      data_frame(ICAO.code = x, airport.name = m_t, longitude = lon, latitude = lat, elevation = ele,
                 Source = "www.aviationweather.gov/docs/metar/stations.txt",
                 Author.of.Source = "Greg Thompson NCAR/RAP")
    } else{
      data_frame(longitude = lon, latitude = lat, elevation = ele,
                 Source = "www.aviationweather.gov/docs/metar/stations.txt",
                 Author.of.Source = "Greg Thompson NCAR/RAP")
    }
  }
}
