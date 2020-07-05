#' Get historical METAR reports fro an airport.
#'
#' Function downloads a set of historical METAR weather reports.
#' The default source the Iowa Environmental Mesonet web page of Iowa State University
#' ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/ \cr
#' The secondary source is Weather Information Service provided by Ogimet http://www.ogimet.com/
#'
#' @param airport character;  ICAO or IATA airport code.
#' @param start_date character; start date.
#' @param end_date character; end date.
#' @param from character; selection of online METAR database, \cr allowed values are "iastate" for  ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/ \cr
#' and "ogimet" for Weather Information Service provided by Ogimet http://www.ogimet.com/.
#'
#' @return A character vector for a current METAR weather report.
#' @return A data frame character vectors with historical METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get_historical("EPWA", start_date = "2017-11-20", end_date = "2017-11-25")
#' metar_get_historical("CYUL", start_date = "2016-07-01", end_date = "2016-07-10", from = "ogimet")
#' metar_get_historical("MAD", start_date = "2000-06-01", end_date = "2000-06-02", from = "iastate")
#'
#'
metar_get_historical <- function(airport = "EPWA",
                                 start_date = "2020-01-01",
                                 end_date = "2020-01-10",
                                 from = "iastate"){

  # try to find ICAO based on IATA
  if(stringr::str_detect(airport, pattern = "^[A-Za-z]{3}$")){
    airport <- metar_iata_icao(airport)
  }

  # check if airport has the correct format
  if(!stringr::str_detect(airport, pattern = "^[A-Za-z]{4}$")){
    stop("ERROR: invalid format of an airport ICAO code!", call. = FALSE)
  }
  # check if dates have correct format
  if(!stringr::str_detect(start_date, pattern = "^\\d{4}[-]\\d\\d[-]\\d\\d$") |
     !stringr::str_detect(end_date, pattern = "^\\d{4}[-]\\d\\d[-]\\d\\d$")){
    stop("ERROR: invalid format of start_date and/or end_date!", call. = FALSE)
  }

  # check if dates range is correct
  if(as.Date(start_date) >= as.Date(end_date)){
    stop("ERROR: star_date is equal or later than end_date!", call. = FALSE)
  }

  syear <- stringr::str_sub(start_date, 1, 4)
  smonth <- stringr::str_sub(start_date, 6, 7)
  sday <- stringr::str_sub(start_date, 9, 10)
  eyear <- stringr::str_sub(end_date, 1, 4)
  emonth <- stringr::str_sub(end_date, 6, 7)
  eday <- stringr::str_sub(end_date, 9, 10)

  if(from == "ogimet"){
    link <- paste0("http://www.ogimet.com/display_metars2.php?lang=en&lugar=",
                   airport,"&tipo=SA&ord=DIR&nil=NO&fmt=txt&ano=",
                   syear, "&mes=",
                   smonth, "&day=",
                   sday, "&hora=00&anof=",
                   eyear, "&mesf=",
                   emonth, "&dayf=",
                   eday, "&horaf=23&minf=59&enviar=Ver")
    cat("Getting information from Weather Information Service http://www.ogimet.com/\n")
    cat("developed by Guillermo Ballester Valor\n")
  } else if(from == "iastate"){
    link <- paste0("mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=", airport,
                   "&data=metar",
                   "&year1=", syear,
                   "&month1=", as.numeric(smonth),
                   "&day1=", as.numeric(sday),
                   "&year2=", eyear,
                   "&month2=", as.numeric(emonth),
                   "&day2=", as.numeric(eday),
                   "&tz=Etc%2FUTC&format=onlycomma&latlon=no&direct=no&report_type=1&report_type=2")
    cat("Iowa Environmental Mesonet web page of Iowa State University\n")
    cat("ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/\n")
  } else {
    stop(paste("Incorrect input parameters from = '" ,from ,
               "'. Please use 'ogimet' or 'iastate'.", sep = ""), call. = FALSE)
  }

  myfile <- RCurl::getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)

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
    ds$x <- stringr::str_trim(ds$x)
    idd <- stringr::str_detect(ds$x, pattern = pattern)
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
    ds[,2] <- stringr::str_replace_all(ds[,2], "[[:punct:]]", "")
    ds[,2] <- stringr::str_replace_all(ds[,2], " ", "")
    ds[,3] <- stringr::str_trim(ds[,3])
    out <- paste(ds[,2], "METAR", ds[,3], sep = " ")
  }
  out
}
