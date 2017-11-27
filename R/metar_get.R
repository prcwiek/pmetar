#' metar_get
#'
#' @param airport
#' @param start_date
#' @param end_date
#'
#' @return
#' @export
#'
#' @examples
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
    cat("History\n")
  } else {
    cat("Incorrect input parameters!\nPlease check!\n")
  }
}
