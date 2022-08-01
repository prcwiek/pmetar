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
#' @param verbose logical; default FALSE
#' 
#' @return if verbose = FALSE, TRUE if a METAR is correct, FALSE if not.
#' @return if verbose = TRUE, all incorrect METAR reports will be printed 
#'
#' @examples
#' metar_is_correct("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_is_correct("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5! SLP180")
#' metar_is_correct("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04//M03 Q1025")
#'
#' @export
#'
metar_is_correct <- function(x, verbose = FALSE) {
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
  
  # check wind speed syntax
  fT <- stringr::str_detect(x, pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS|\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT)")
  out[!fT] <- FALSE

  # check wind direction syntax
  fT <- stringr::str_detect(x, pattern = "(\\s\\d{5}G\\d+KT|\\s\\d{5}KT|\\s\\d{5}MPS)")
  out[!fT] <- FALSE
  fT <- stringr::str_detect(x, pattern = "(\\d{4,}V\\d{4,}|\\d{3}V\\d{4,}|\\d{4,}V\\d{3})")
  out[fT] <- FALSE

  if (verbose) {
    if (length(out)/sum(out) != 1) {
      message("Incorrect METAR reports:")
      x[!out]  
    } else {
      if (length(x) == 1) {
        message("METAR report is correct!")
      } else {
        message("All METAR reports are correct!")
      }
    }
  } else {
    as.logical(out)  
  }
}
