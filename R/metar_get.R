#' Get a current METAR reports or historical METAR reports.
#'
#' Function geta metar report from a web page.\cr
#' \cr
#' If only airport is passed as an argument, a current METAR weather report is donwloaded from\cr
#' the web page https://aviationweather.gov/metar/\cr
#' \cr
#' If a start date and an end date are specified, a set of historical METAR weather reports\cr
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
#' metar_get("EPWA")
#' metar_get("CYUL")
#' metar_get("LEMD")
#' metar_get("EPWA", start_date = "2017-11-20", end_date = "2017-11-25")
#' metar_get("CYUL", start_date = "2016-07-01", end_date = "2016-07-10")
#' metar_get("LEMD", start_date = "2000-06-01", end_date = "2000-06-02")
#'
metar_get <- function(airport, start_date = "", end_date = ""){
  if((start_date == "" | end_date == "") & airport != "") {
    link <- paste(
      "https://aviationweather.gov/metar/data?ids=",
      airport,
      "&format=raw&date=0&hours=0",
      sep = ""
    )
    myfile <- getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
    mydata <- read.csv((textConnection(myfile)))
    ind <- which(mydata[,1] == "<!-- Data starts here -->") + 1
    metar <- as.character(mydata[ind,1])
    if(str_detect(metar, pattern = "No METAR found")){
      cat(paste("No METAR found for ", airport, "!\n", sep = ""))
    } else str_split(metar, "<br", simplify = TRUE)[1]
  } else if(str_detect(start_date, pattern = START %R% one_or_more(DGT) %R% "-" %R% one_or_more(DGT) %R% "-" %R% one_or_more(DGT)) &
            str_detect(end_date, pattern = START %R% one_or_more(DGT) %R% "-" %R% one_or_more(DGT) %R% "-" %R% one_or_more(DGT)) &
            airport != "" &
            as.Date(end_date) > as.Date(start_date)) {
    #cat("History\n")
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
    pattern <- START %R% one_or_more(DGT) %R% SPC %R% one_or_more(WRD)
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
  } else {
    cat("Incorrect input parameters!\nPlease check!\n")
  }
}
