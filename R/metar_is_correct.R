#' Check if METAR report is correct.
#' 
#' Function checks METRAR reports syntax.
#' 
#' Verify if reports include not allowed elements and characters:\cr
#' ! \ ? . , ; : * # & ' " ) \cr
#' METAR SPECI\cr
#' multiple slash\cr
#' 
#' Also it checks if an airport code is the first element or appear
#' immediately after METAR, SPECI, METAR COR ro SPECI COR.
#' 
#' @param x character vector; METAR weather report or reports.
#' 
#' @return TRUE if a METAR is correct, FALSE if not.
#'
#' @examples
#' metar_is_correct("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_is_correct("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5! SLP180")
#' metar_is_correct("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04//M03 Q1025")
#'
#' @export
#'
metar_is_correct <- function(x) {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  out[1:length(x)] <- TRUE
  
  # check not allowed characters
  fT <- stringr::str_detect(x, pattern = "(\\!|\\?|\\.|\\,|\\;|\\:|\\*|\\#|\\&|\\')")
  out[fT] <- FALSE
  fT <- stringr::str_detect(x, pattern = '\\"')
  out[fT] <- FALSE
  
  # check if the string METAR SPECI appear
  fT <- stringr:: str_detect(x, pattern = "METAR SPECI")
  out[fT] <- FALSE
  
  # check if the airport code is placed in the correct place
  fT <- stringr::str_detect(x, pattern = "((METAR|SPECI|METAR COR|SPECI COR)\\s[A-Z]{4}\\s|^[A-Z]{4}\\s\\d{6}[A-Z]\\s)")
  out[!fT] <- FALSE
  
  # check duplicated /
  fT <- stringr:: str_detect(x, pattern = "/{2,}")
  out[fT] <- FALSE
  
  
  as.logical(out)
}
