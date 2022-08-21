#' Check if METAR report is correct.
#' 
#' Function checks METRAR reports syntax.
#' 
#'It checks:\cr
#' appearance of not allowed characters: ! \ ? . , ; : * # & ' " ) and 
#' multiple slash characters\cr
#' wind speed syntax\cr
#' wind direction syntax\cr
#' pressure syntax\cr
#' air and dew point temperature syntax\cr
#' if an airport code is the first element or appear
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
  
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  
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

  # check wind speed and gust syntax
  fT <- stringr::str_detect(x, pattern = "(\\d{5}(MPS|G\\d{2}MPS)|VRB\\d{2}MPS|\\d{5}(KT|G\\d{2}KT)|VRB\\d{2}KT|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS|\\d{3}P49MPS|\\d{3}P99KT)")
  out[!fT] <- FALSE
  fT <- stringr::str_detect(x, pattern = "()")

  # check wind direction syntax
  fT <- stringr::str_detect(x, pattern = "(\\s\\d{5}G\\d+KT|\\s\\d{5}KT|\\s\\d{5}MPS|VRB[\\d]+KT|[\\d]+MPS|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS|\\d{3}P49MPS|\\d{3}P99KT)")
  out[!fT] <- FALSE
  fT <- stringr::str_detect(x, pattern = "(\\s\\d{4,}V\\d{4,}\\s|\\s\\d{3}V\\d{4,}\\s|\\s\\d{4,}V\\d{3}\\s)")
  out[fT] <- FALSE
  
  # check pressure syntax
  fT <- stringr::str_detect(x, pattern = "(\\sQ\\d{4}|\\sA\\d{4})")
  out[!fT] <- FALSE
 
  # check air and dew point temperature syntax
  fT <- stringr::str_detect(x, pattern = "(\\s\\d{2}/\\d{2}\\s|\\s\\d{2}/M\\d{2}\\s|M\\d{2}/\\d{2}\\s|\\sM\\d{2}/M\\d{2}\\s)")
  out[!fT] <- FALSE
  
  if (verbose) {
    if (length(out)/sum(out) != 1) {
      if (length(x) == 1 | (length(out)/(sum(out) + 1)) == 1) {
        message("Incorrect METAR report:")  
      } else {
        message("Incorrect METAR reports:")  
      }
      for (item in which(!out)) {
        cat(item, "\t", x[item], "\n")
      }
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
