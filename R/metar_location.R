#' Extract approximated airport location.
#'
#' Function extract approximated airport lattitude, longitude and elevation for a given airport code.
#'
#' @param x Input character vector
#'
#' @return A data frame with three columns longitude, latitude, elevation in meters and one row.
#'
#' @export
#'
#' @examples
#' metar_location("EPWA")
#' metar_location("CYUL")
#' metar_location("LEMD")
#'
metar_location <- function(x) {
  if(nchar(x) == 0){
    cat("Airport not specified!\n")
  } else if (sum(str_count(mst, pattern = x)) < 1) {
    cat(paste("Airport ", x, " not found!\n", sep = ""))
  } else {
    if(sum(str_count(mst, pattern = paste(x, "[\\s]+[\\d]+[\\s]+[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))) == 1) {
      m_t <- str_extract(mst, paste(x, "[\\s]+[\\d]+[\\s]+[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
    } else {
      m_t <- str_extract(mst, pattern = paste(x, "[\\s]+[\\w]+[\\s]+[\\d]+[\\s]+[\\d]+\\s[\\d]+(?:N|S)[\\s]+[\\d]+\\s[\\d]+(?:E|W)[\\s]+[\\d]+", sep = ""))
    }
    m_t <- m_t[!is.na(m_t)]
    lat <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:N|S)")
    lat <- str_sub(lat, 1, nchar(lat) - 1)
    lat <- as.numeric(str_replace(lat, " ", "."))
    lon <- str_extract(m_t, pattern = "[\\d]+\\s[\\d]+(?:E|W)")
    lon <- str_sub(lon, 1, nchar(lon) - 1)
    lon <- as.numeric(str_replace(lon, " ", "."))
    ele <- as.numeric(str_extract(m_t, pattern = "[\\d]+$"))
    data.frame(longitude = lon, latitude = lat, elevation = ele, stringsAsFactors = FALSE)
  }
}
