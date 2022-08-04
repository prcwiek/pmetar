#' Get wind direction.
#'
#' Extract a wind direction value from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param numeric_only logical; the default value is FALSE and information about variability will be included.
#' If TRUE only a numeric value of direction will be returned.
#' @param check logical; if TRUE the syntax of METAR reports will be checked.
#'
#' @return a numeric vector with a wind direction in degrees.
#'
#' @export
#'
#' @examples
#' metar_dir("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_dir("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180",
#' numeric_only = TRUE)
#' metar_dir("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#'
metar_dir <- function(x, numeric_only = FALSE, check = FALSE){
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  out <- c(1:length(x))
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  if (check) {
    icorrect <- metar_is_correct(x)
    if (numeric_only) {
      out[which(!icorrect)] <- NA  
    } else {
      out[which(!icorrect)] <- ""       
    }
  } else {
    icorrect <- rep(TRUE, length(x))
  }
  x <- x[which(icorrect)]
  outx <- c(1:length(x))
  if(numeric_only) {
    outx[1:length(x)] <- NA
    # look for nnnnnGnnKT or nnnnnKT or nnnnnnMPS
    fT <- stringr::str_detect(x, pattern = "(\\s\\d{5}G\\d+KT|\\s\\d{5}KT|\\s\\d{5}MPS|\\s\\d{3}P49MPS|\\s\\d{3}P99KT)")
    outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(\\d{5}G\\d+KT|\\d{5}KT|\\d{5}MPS|\\d{3}P49MPS|\\d{3}P99KT)"), 1, 3))
    
    out[which(icorrect)] <- outx
    out
  } else {
    outx[1:length(x)] <- ""
    # look for nnnnnGnnKT or nnnnnKT or nnnnnnMPS
    fT <- stringr::str_detect(x, pattern = "(\\s\\d{5}G\\d+KT|\\s\\d{5}KT|\\s\\d{5}MPS|\\s\\d{3}P49MPS|\\s\\d{3}P99KT)")
    outx[fT] <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "(\\d{5}G\\d+KT|\\d{5}KT|\\d{5}MPS|\\d{3}P49MPS|\\d{3}P99KT)"), 1, 3))
    # look for nnnVnnn
    fT <- stringr::str_detect(x, pattern = "\\s\\d{3}V\\d{3}\\s")
    outx[fT] <- paste0(outx[fT], "; variable from ",
                      as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\d{3}V\\d{3}"), 1, 3)),
                      " to ",
                      as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "\\d{3}V\\d{3}"), -3, -1))
    )
    fT <- stringr::str_detect(x, pattern = "(VRB[\\d]+KT|VRB[\\d]+MPS|VRB[\\d]+G[\\d]+KT|VRB[\\d]+G[\\d]+MPS)")
    outx[fT] <- "Variable"
    out[which(icorrect)] <- outx
    out
  }
}
