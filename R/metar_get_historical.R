#' Get historical METAR reports.
#'
#' Function download a set of historical METAR weather reports.
#' The default source the Iowa Environmental Mesonet web page of Iowa State University
#' ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/ \cr
#' The secondary source is Weather Information Service provided by Ogimet http://www.ogimet.com/
#'
#' @param airport Input character vector
#' @param start_date Input character vector
#' @param end_date Input character vector
#' @param from Input character vector, allowed values are "iastate" and "ogimet"
#'
#' @return A character vector for a current METAR weather report.
#' @return A data frame character vectors with historical METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get_historical("EPWA", start_date = "2017-11-20", end_date = "2017-11-25")
#' metar_get_historical("CYUL", start_date = "2016-07-01", end_date = "2016-07-10", from = "ogimet")
#' metar_get_historical("LEMD", start_date = "2000-06-01", end_date = "2000-06-02", from = "iastate")
#'
#'
metar_get_historical <- function(airport = "EPWA",
                                 start_date = "2017-11-21",
                                 end_date = "2017-11-22",
                                 from = "iastate"){

  # check if dates have correct format
  if(!str_detect(start_date, pattern = "^\\d\\d\\d\\d[-]\\d\\d[-]\\d\\d$") |
     !str_detect(end_date, pattern = "^\\d\\d\\d\\d[-]\\d\\d[-]\\d\\d$")){
    stop("ERROR: invalid format of start_date and/or end_date!", call. = FALSE)
  }

  # check if dates range is correct
  if(as.Date(start_date) >= as.Date(end_date)){
    stop("ERROR: star_date is equal or later than end_date!", call. = FALSE)
  }

  syear <- str_sub(start_date, 1, 4)
  smonth <- str_sub(start_date, 6, 7)
  sday <- str_sub(start_date, 9, 10)
  eyear <- str_sub(end_date, 1, 4)
  emonth <- str_sub(end_date, 6, 7)
  eday <- str_sub(end_date, 9, 10)

  if(from == "ogimet"){
    link <- paste(
      "http://www.ogimet.com/display_metars2.php?lang=en&lugar=",
      airport,"&tipo=SA&ord=DIR&nil=NO&fmt=txt&ano=",
      syear, "&mes=",
      smonth, "&day=",
      sday, "&hora=00&anof=",
      eyear, "&mesf=",
      emonth, "&dayf=",
      eday, "&horaf=23&minf=59&enviar=Ver",
      sep = "")
  } else if(from == "iastate"){
    link <- paste(
      "mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=", airport,
      "&data=metar",
      "&year1=", syear,
      "&month1=", as.numeric(smonth),
      "&day1=", as.numeric(sday),
      "&year2=", eyear,
      "&month2=", as.numeric(emonth),
      "&day2=", as.numeric(eday),
      "&tz=Etc%2FUTC&format=onlycomma&latlon=no&direct=no&report_type=1&report_type=2",
      sep = "")
  } else {
    stop(paste("Incorrect input parameters from = '" ,from ,
               "'. Please use 'ogimet' or 'iastate'.", sep = ""), call. = FALSE)
  }

  myfile <- getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)

  if(myfile == "ERROR: Malformed Date!"){
    stop(paste("Message from mesonet.agron.iastate.edu :", "ERROR: Malformed Date!"), call. = FALSE)
  }

  ds <- read.csv((textConnection(myfile)), stringsAsFactors = FALSE)

  if(from == "ogimet"){
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
  } else {#if(from == "iastate") {
    ds[,2] <- str_replace_all(ds[,2], "[[:punct:]]", "")
    ds[,2] <- str_replace_all(ds[,2], " ", "")
    ds[,3] <- str_trim(ds[,3])
    out <- paste(ds[,2], "METAR", ds[,3], sep = " ")
  }
}
