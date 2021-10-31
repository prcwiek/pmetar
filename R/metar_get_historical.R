#' Get historical METAR reports.
#'
#' Download a set of historical METAR weather reports.
#' The default source is the Iowa Environmental
#' Mesonet web page of Iowa State University ASOS-AWOS-METAR\cr
#' [https://mesonet.agron.iastate.edu/AWOS/](https://mesonet.agron.iastate.edu/AWOS/)\cr
#' The secondary source of METAR reports is Weather Information Service provided by Ogimet
#' [https://www.ogimet.com/](https://www.ogimet.com/). However for this source
#' the requested period is limited to 31 days. METAR reports are available from
#' the year 2005.
#'
#' @param airport character;  ICAO or IATA airport code.
#' @param start_date character; start date in the format YYYY-MM-DD.
#' @param end_date character; end date in the format YYYY-MM-DD.
#' @param from character; selection of online METAR database, \cr
#' the default value is "iastate" downolading METAR reports from
#' Iowa Environmental Mesonet ASOS-AWOS-METAR
#' [https://mesonet.agron.iastate.edu/AWOS/](https://mesonet.agron.iastate.edu/AWOS/). \cr
#' Setting the parameter from to "ogimet" allows to use Weather Information Service provided by Ogimet
#' [https://www.ogimet.com/](https://www.ogimet.com/).
#'
#' @return a data frame character vectors with historical METAR weather report.
#'
#' @export
#'
#' @examples
#' \donttest{
#' metar_get_historical("EPWA", start_date = "2017-11-20", end_date = "2017-11-25")
#' metar_get_historical("MAD", start_date = "2015-06-01", end_date = "2015-06-02",
#' from = "iastate")
#' metar_get_historical("CYUL", start_date = "2016-07-01", end_date = "2016-07-05",
#' from = "ogimet")
#' }
#'
metar_get_historical <- function(airport = "EPWA",
                                 start_date = "2020-01-01",
                                 end_date = "2020-01-10",
                                 from = "iastate"){

  # check if x is a data frame
  if(is.data.frame(airport)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }

  # check if x is a character with length 1
  if(length(airport) > 1){
    stop("pmetar package error: Only one airport at once!", call. = FALSE)
  }

  # check whether airpot consist of spaces
  if(stringr::str_detect(airport, pattern = "\\s")) {
    stop("pmetar package error: Airport code contains blank(s)!", call. = FALSE)
  }

  # try to find ICAO based on IATA
  if(stringr::str_detect(airport, pattern = "^[A-Za-z]{3}$")){
    airport <- metar_iata_icao(airport)
  }

  # check if airport has the correct format
  if(!stringr::str_detect(airport, pattern = "^[A-Za-z]{4}$")){
    stop("pmetar package error: invalid format of an airport ICAO or IATA code!", call. = FALSE)
  }
  # check if dates have correct format
  if(!stringr::str_detect(start_date, pattern = "^\\d{4}[-]\\d\\d[-]\\d\\d$") |
     !stringr::str_detect(end_date, pattern = "^\\d{4}[-]\\d\\d[-]\\d\\d$")){
    stop("pmetar package error: invalid format of start_date and/or end_date!", call. = FALSE)
  }

  # check if dates range is correct
  if(as.Date(start_date) >= as.Date(end_date)) {
    stop("pmetar package error: start_date is equal or later than end_date!", call. = FALSE)
  }

  # check the maximum period of 31 days for Ogimet web page
  if(from == "ogimet" & (as.Date(end_date) - as.Date(start_date) > 31)) {
    stop("pmetar package error: Period longer than 31 days for the Ogimet source!", call. = FALSE)
  }

  # check if start_date is not earlier than January 2005 for Ogimet web page
  if(from == "ogimet" & !(as.Date(start_date) >= as.Date("2005-01-01"))) {
    stop("pmetar package error: Start date earlier than 2005-01-01 for the Ogimet source!", call. = FALSE)
  }

  syear <- stringr::str_sub(start_date, 1, 4)
  smonth <- stringr::str_sub(start_date, 6, 7)
  sday <- stringr::str_sub(start_date, 9, 10)


  if(from == "ogimet"){
    eyear <- stringr::str_sub(end_date, 1, 4)
    emonth <- stringr::str_sub(end_date, 6, 7)
    eday <- stringr::str_sub(end_date, 9, 10)
    link <- paste0("www.ogimet.com/display_metars2.php?lang=en&lugar=",
                   airport,"&tipo=SA&ord=DIR&nil=NO&fmt=txt&ano=",
                   syear, "&mes=",
                   smonth, "&day=",
                   sday, "&hora=00&anof=",
                   eyear, "&mesf=",
                   emonth, "&dayf=",
                   eday, "&horaf=23&minf=59&enviar=Ver")
    message("Getting information from Weather Information Service http://www.ogimet.com/")
    message("developed by Guillermo Ballester Valor")
  } else if(from == "iastate"){
    end_date <- as.Date(end_date)
    eyear <- lubridate::year(end_date + 1)
    emonth <- lubridate::month(end_date + 1)
    eday <- lubridate::day(end_date + 1)
    link <- paste0("mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=", airport,
                   "&data=metar",
                   "&year1=", syear,
                   "&month1=", smonth,
                   "&day1=", sday,
                   "&year2=", eyear,
                   "&month2=", emonth,
                   "&day2=", eday,
                   "&tz=Etc%2FUTC&format=onlycomma&latlon=no&direct=no&report_type=1&report_type=2")
    message("Iowa Environmental Mesonet web page of Iowa State University")
    message("ASOS-AWOS-METAR http://mesonet.agron.iastate.edu/AWOS/")
  } else {
    stop(paste("Incorrect input parameters from = '" ,from ,
               "'. Please use 'ogimet' or 'iastate'.", sep = ""), call. = FALSE)
  }

  # Function for handling problems with the server
  check_server_status <- function(p) {
    tryCatch(
      httr::GET(p, config = httr::timeout(20)),
      error = function(e) conditionMessage(e),
      warning = function(w) conditionMessage(w)
    )
  }

  # Check internet connection
  if(!curl::has_internet()) {
    message("No internet connection!")
    return(invisible(NULL))
  }

  server_link <- stringr::str_extract(link, pattern = ".+?/")
  server_answer <- check_server_status(server_link)

  # Check timeout problems
  if(class(server_answer) != "response") {
    message(server_answer)
    return(invisible(NULL))
  }

  # Check status > 400
  if(httr::http_error(server_answer)) {
    httr::message_for_status(server_answer)
    return(invisible(NULL))
  }

  tryCatch(
    expr = {
      myfile <- RCurl::getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
    },
    error = function(e){
      if (from == "ogimet") {
        stop("pmetar package error: Cannot connect to the server www.ogimet.com!", call. = FALSE)
      } else {
        stop("pmetar package error: Cannot connect to the server mesonet.agron.iastate.edu!", call. = FALSE)
      }
    }
  )

  ds <- utils::read.csv((textConnection(myfile)), stringsAsFactors = FALSE)

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

  # check out consists of data for mesonet.agron.iastate.edu
  if(from == "iastate" & out[1] == " METAR ") {
    stop("pmetar package error: Data not available on mesonet.agron.iastate.edu!", call. = FALSE)
  }

  message("Don't use for flight planning or navigation!")
  out
}
