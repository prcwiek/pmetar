#' Get historical METAR reports.
#'
#' Function download a  a set of historical METAR weather reports\cr
#' is is downloaded from the web page http://www.ogimet.com/
#'
#' @param airport Input character vector
#' @param start_date Input character vector
#' @param end_date Input character vector
#'
#' @return A character vector for a current METAR weather report.
#' @return A data frame character vectors with historical METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get_historical("EPWA", start_date = "2017-11-20", end_date = "2017-11-25")
#' metar_get_historical("CYUL", start_date = "2016-07-01", end_date = "2016-07-10")
#' metar_get_historical("LEMD", start_date = "2000-06-01", end_date = "2000-06-02")
#'
#'
metar_get_historical <- function(airport = "EPWA", start_date = "2017-11-21", end_date = "2017-11-22"){
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
    eday, "&horaf=23&minf=59&enviar=Ver",
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
  pattern <- "^[\\d]+\\s(?:METAR|SPECI)"
  ds$x <- str_trim(ds$x)
  idd <- str_detect(ds$x, pattern = pattern)
  i <- 1
  out <- data.frame(metar = "empty", stringsAsFactors = FALSE)
  metartext <- "empty"
  while(i < length(idd)){
    if(idd[i]){
      out <- rbind(out, metartext)
      metartext <- ds[i,1]
    } else metartext <- paste(metartext, ds[i,1])
    i <- i + 1
  }
  out <- out[3:nrow(out),]
}
