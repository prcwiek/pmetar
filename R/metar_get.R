#' Get a current METAR report for an airport.
#'
#' Function geta metar report from a web page.\cr
#' \cr
#' If only airport is passed as an argument, a current METAR weather report is donwloaded from\cr
#' the web page https://aviationweather.gov/metar/\cr
#' \cr
#'
#' @param airport Input character vector with an ICAO airport code
#'
#' @return A character vector with a current METAR weather report.
#'
#' @export
#'
#' @examples
#' metar_get("EPWA")
#' metar_get("CYUL")
#' metar_get("LEMD")
#'
metar_get <- function(airport = "EPWA"){
  if(str_detect(airport, pattern = "^[A-Za-z]{4}$")) {
    cat("Getting information from Aviation Weather Center www.aviationweather.gov/metar\n")
    link <- paste0("https://aviationweather.gov/metar/data?ids=",
                   airport,
                   "&format=raw&date=0&hours=0")
    myfile <- getURL(link, ssl.verifyhost = FALSE, ssl.verifypeer = FALSE)
    mydata <- read.csv((textConnection(myfile)))
    ind <- which(mydata[,1] == "<!-- Data starts here -->") + 1
    metar <- as.character(mydata[ind,1])
    if(str_detect(metar, pattern = "No METAR found")){
      cat(paste("No METAR found for ", airport, "!\n", sep = ""))
    } else str_split(metar, "<br", simplify = TRUE)[1]
  } else {
    cat("Incorrect ICAO airport code!\n")
  }
}
