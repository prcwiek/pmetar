#' Get weather conditions information.
#'
#' Extract and parse weather conditions information METAR WX codes.
#'
#' @param x Input character vector
#' @param sep character; comma or semicolon, used for separating decoded elements of weather
#' conditions information.
#'
#' @return A character vector. with METAR WX codes.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' metar_wx_codes("METAR EPWA 132100Z 29006KT 260V320 8000 SHRA SCT009 BKN025CB 18/17 Q1011")
#' metar_wx_codes("CYUL 101900Z 27015G25KT 15SM DRSN SCT028 BKN090 OVC110 M04/M10 A2973 RMK")
#' metar_wx_codes("METAR EPKK 200300Z 23014KT 9999 -SHSN SCT009CB BKN012 01/M00 Q1008", sep = ",")
#' metar_wx_codes("202001190045 METAR KEWR 190045Z 19008KT 4SM -RA -PL BR FEW007 01/M01 A2995")
#'
metar_wx_codes <- function(x, sep = ";") {
  # Check if x is a data frame and stop if yes
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # Check sep values
  if (!stringr::str_detect(sep, pattern = "(^;$|^,$)")) {
    stop("pmetar package error: Invalid sep value! It must be comma or semicolon!")
  }
  # Function matches an extracted code to a description from metarWXcodes data frame
  wx_code_resolve <- function(xcr){
    out_cr <- xcr
    for (i in 1:length(xcr)) {
      temp <- unlist(stringr::str_extract_all(as.character(xcr[i]), ".{2}"))
      out_cr[i] <- stringr::str_c(sapply(temp, function(y) metarWXcodes$Meaning[match(y, metarWXcodes$Abbreviation)]),
                                  collapse = " ")
    }
    out_cr
  }

  # Function single WX codes from a METAR weather report
  wx_code_extract <- function(xce) {
    xce <- as.data.frame(xce, stringsAsFactors = FALSE)
    wx_extracted <- xce
    wx_extracted[1:nrow(wx_extracted),] <- ""
    wx_extracted[apply(xce, 2, function(x) stringr::str_detect(x, pattern = paste0("[+](", pattern_abbrev, ")")))] <- "Heavy intensity:"
    wx_extracted[apply(xce, 2, function(x) stringr::str_detect(x, pattern = paste0("[-](", pattern_abbrev, ")")))] <- "Light intensity:"
    wx_extracted[apply(xce, 2, function(x) stringr::str_detect(x, pattern = paste0("RE(", pattern_abbrev, ")")))] <- "Recent:"
    if (nrow(xce) == 1) {
      xce <- t(apply(xce, 1, function(x) stringr::str_replace(x, "([+]|[-]|RE|\\s)", "")))
      wx_resolved <- t(apply(xce, 1, function(x) wx_code_resolve(x)))
    } else if (nrow(xce) != 0) {
      xce <- apply(xce, 2, function(x) stringr::str_replace(x, "([+]|[-]|RE|\\s)", ""))
      wx_resolved <- apply(xce, 2, function(x) wx_code_resolve(x))
    } else {
      wx_resolved = matrix(data = "", nrow = 1, ncol = 1)
    }
    out_ce <- c(1:nrow(wx_extracted))
    for (j in 1:nrow(wx_extracted)) {
      out_ce[j] <- paste(wx_extracted[j,], wx_resolved[j,], collapse = paste0(sep, " ")) #collapse = "; "
      out_ce[j] <- stringr::str_replace_all(as.character(out_ce[j]),
                                            pattern = paste0(sep, "  "),
                                            replacement = paste0(sep, " ")) #pattern = ";  ", replacement = "; ")
      out_ce[j] <- stringr::str_trim(as.character(out_ce[j]), side = "left")
      out_ce[j] <- stringr::str_replace(as.character(out_ce[j]), pattern = "([;\\s]+$|[,\\s]+$)", replacement =  "")
    }
    out_ce
  }

  # Extract WX codes
  wx_codes <- metarWXcodes %>%
    dplyr::filter(Type != "Intensity") %>%
    dplyr::filter(Type != "Time") %>%
    dplyr::filter(Abbreviation != "")

  out <- c(1:length(x))
  out[1:length(x)] <- ""
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  
  pattern_abbrev <- apply(wx_codes, 2, paste, collapse = "|")[2]

  # Remove a remark (RMK) part of a METAR weather report
  x <- stringr::str_split(x, " RMK", simplify = TRUE)[,1]

  # Remove a forecast part after the forecast change indicator TEMPO
  x <- stringr::str_split(x, " TEMPO", simplify = TRUE)[,1]

  # Remove a forecast part after the forecast change indicator TEMPO
  x <- stringr::str_split(x, " FM", simplify = TRUE)[,1]

  fT <- stringr::str_detect(x, pattern = paste0("(\\s|[+]|[-]|RE)(",
                                                pattern_abbrev,
                                                ")+"))
  out[fT] <- wx_code_extract(stringr::str_extract_all(x[fT],
                                                      pattern = paste0("(\\s|[+]|[-]|RE)(",
                                                                       pattern_abbrev,")+"),
                                                      simplify = TRUE))
  out
}
