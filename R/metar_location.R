#' metar_location
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_location <- function(x) {
  if(nchar(x) == 0){
    cat("Airport not specified!\n")
  } else if (sum(str_count(mst, pattern = x %R% one_or_more(SPC) %R% one_or_more(DGT) %R% one_or_more(SPC)
                           %R% one_or_more(DGT) %R% SPC %R% one_or_more(DGT) %R% or("N", "S")
                           %R% one_or_more(SPC) %R% one_or_more(DGT) %R% SPC %R% one_or_more(DGT)
                           %R% or("E", "W") %R% one_or_more(SPC) %R% one_or_more(DGT))) < 1) {
    cat(paste("Airport ", x, " not found!\n", sep = ""))
  } else {
    m_t <- str_extract(mst, pattern = x %R% one_or_more(SPC) %R% one_or_more(DGT) %R% one_or_more(SPC)
                       %R% one_or_more(DGT) %R% SPC %R% one_or_more(DGT) %R% or("N", "S")
                       %R% one_or_more(SPC) %R% one_or_more(DGT) %R% SPC %R% one_or_more(DGT)
                       %R% or("E", "W") %R% one_or_more(SPC) %R% one_or_more(DGT))
    m_t <- m_t[!is.na(m_t)]
    lat <- str_extract(m_t, pattern = one_or_more(DGT) %R% SPC %R% one_or_more(DGT) %R% or("N", "S"))
    lat <- str_sub(lat, 1, nchar(lat) - 1)
    lat <- as.numeric(str_replace(lat, " ", "."))
    lon <- str_extract(m_t, pattern = one_or_more(DGT) %R% SPC %R% one_or_more(DGT) %R% or("E", "W"))
    lon <- str_sub(lon, 1, nchar(lon) - 1)
    lon <- as.numeric(str_replace(lon, " ", "."))
    ele <- as.numeric(str_extract(m_t, pattern = one_or_more(DGT) %R% END))

    data.frame(longitude = lon, latitude = lat, elevation = ele, stringsAsFactors = FALSE)
  }
}
