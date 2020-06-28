#' Extract METAR WX codes.
#'
#' Function extracts METAR WX codes from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A charcter vector. with METAR WX codes.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' metar_wx_codes("201807132100 METAR EPWA 132100Z 29006KT 260V320 8000 SHRA SCT009 BKN025CB 18/17 Q1011 NOSIG")
#' metar_wx_codes("CYUL 101900Z 27015G25KT 15SM DRSN SCT028 BKN090 OVC110 M04/M10 A2973 RMK")
#' metar_wx_codes("201711200300 METAR EPKK 200300Z 23014KT 9999 -SHSN SCT009CB BKN012 01/M00 Q1008=")
#'
metar_wx_codes <- function(x) {

  wx_code_resolve <- function(t){
    stringr::str_c(metarWXcodes$Meaning[match(t, metarWXcodes$Abbreviation)], collapse = ", ")
  }

  outwx <- c(1:length(x))
  outwx[1:length(x)] <- ""
  tempwx <- c(1:length(x))
  tempwx[1:length(x)] <- ""
  wx_codes <- metarWXcodes %>%
    dplyr::filter(Type != "Intensity") %>%
    dplyr::filter(Type != "Time") %>%
    dplyr::filter(Abbreviation != "")

  pattern_abbrev <- apply(wx_codes, 2, paste, collapse = "|")[2]

  fT <- stringr::str_detect(x, pattern = paste0("(\\s|[+]|[-]|RE)[(",
                                       pattern_abbrev,
                                       ")]+\\s"))
  if(sum(fT) > 0){
    tempwx <- stringr::str_trim(stringr::str_extract(x, pattern = paste0("(\\s|[+]|[-]|RE)[(",
                                                       pattern_abbrev,
                                                       ")]+\\s")))

    outwx[stringr::str_detect(x, pattern = paste0("[+](", pattern_abbrev, ")"))] <- "Heavy intensity: "
    outwx[stringr::str_detect(x, pattern = paste0("[-](", pattern_abbrev, ")"))] <- "Light intensity: "
    outwx[stringr::str_detect(x, pattern = paste0("RE(", pattern_abbrev, ")"))] <- "Recent "

    tempwx[fT] <- stringr::str_replace(tempwx[fT], "[+]", "")
    tempwx[fT] <- stringr::str_replace(tempwx[fT], "[-]", "")
    tempwx[fT] <- stringr::str_replace(tempwx[fT], "RE", "")

    tempwx[fT] <- stringr::str_extract_all(tempwx[fT], ".{2}")

    outwx[fT] <- paste0(outwx[fT], sapply(tempwx[fT], wx_code_resolve))
  }
  outwx
}
