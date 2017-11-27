#' metar_get_historical
#'
#' @param airport
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
metar_get_historical <- function(airport, start_date = "2017-11-21", end_date = "2017-11-22"){
  syear <- str_sub(start_date, 1, 4)
  smonth <- str_sub(start_date, 6, 7)
  sday <- str_sub(start_date, 9, 10)
  eyear <- str_sub(end_date, 1, 4)
  emonth <- str_sub(end_date, 6, 7)
  eday <- str_sub(end_date, 9, 10)
  link <- paste(
    "http://www.ogimet.com/display_metars2.php?lang=en&lugar=",
    airport,"&tipo=SA&ord=DIR&nil=NO&fmt=txt&ano=",
    syear, "&mes=",
    smonth, "&day=",
    sday, "&hora=00&anof=",
    eyear, "&mesf=",
    emonth, "&dayf=",
    eday, "&horaf=23&minf=59&send=send",
    sep = ""
  )
  myfile <- getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
  #mydata <- read.csv((textConnection(myfile)))
  ds <- read.csv((textConnection(myfile)), stringsAsFactors = FALSE)
  text_pattern <- paste("#  METAR/SPECI from", airport)
  ids <- which(ds[,1] == text_pattern) + 2
  ide <- which(ds[,1] == "</pre>") - 1
  ds <- as.data.frame(ds[ids:ide,1], stringsAsFactors = FALSE)
  colnames(ds) <- c("x")
  pattern <- START %R% one_or_more(DGT) %R% SPC %R% one_or_more(WRD)
  idx <- which(is.na(str_match(ds$x, pattern = pattern)))
  idy <- which(!is.na(str_match(ds$x, pattern = pattern)))
  ds$y <- ds$x
  ds[idy,2] <- paste(str_trim(ds[idy,2]), str_trim(ds[idx,1]))
  dl <- !is.na(as.data.frame((str_match(ds$y, pattern = pattern))))
  out <- as.data.frame(ds[dl,2], stringsAsFactors = FALSE)
  colnames(out) <- c("metar")
  out
}
