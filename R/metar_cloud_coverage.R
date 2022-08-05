#' Get cloud coverage information.
#'
#' Extract and parse cloud coverage information from METAR weather report.
#'
#' @param x character vector; a METAR weather report or reports.
#' @param sep character; comma or semicolon, used for separating decoded elements of weather
#' conditions information.
#'
#' @return a character vector with cloud coverage information.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' metar_cloud_coverage("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_cloud_coverage("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_cloud_coverage("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_cloud_coverage("KEWR 011451Z 26015KT 10SM FEW030 FEW045 BKN065 04/M07 A2977", sep = ",")
#'
metar_cloud_coverage <- function(x, sep = ";") {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # Check sep values
  if (!stringr::str_detect(sep, pattern = "(^;$|^,$)")) {
    stop("pmetar package error: Invalid sep value! It must be comma or semicolon!")
  }
  # function for extracting several repeating elements, like FEW030 FEW045
  multi_extracting <- function(tdist, tpattern) {
    to_remove_1 <- stringr::str_extract(tpattern, pattern = "^[A-Z]{3}")
    to_remove_2 <- stringr::str_extract(tpattern, pattern = "[A-Z]{2}$")
    if(tpattern == "BKN\\/{3}") {
      to_remove_1 <- paste0(to_remove_1, "\\/{3}")
    }
    if(is.na(to_remove_2)) {
      dist <- tdist %>%
        dplyr::mutate_if(is.character, stringr::str_remove, pattern = to_remove_1) %>%
        dplyr::mutate_if(is.character, as.numeric)
    } else {
      dist <- tdist %>%
        dplyr::mutate_if(is.character, stringr::str_remove, pattern = to_remove_1) %>%
        dplyr::mutate_if(is.character, stringr::str_remove, pattern = to_remove_2) %>%
        dplyr::mutate_if(is.character, as.numeric)
    }
    dist <- dist * 100
    dist_m <- dist * 0.3048
#    dist <- tidyr::unite(dist, "ft", sep = "; ", na.rm = TRUE)
    dist <- tidyr::unite(dist, "ft", sep = paste0(sep, " "), na.rm = TRUE)
#    dist_m <- tidyr::unite(dist_m, "m", sep = "; ", na.rm = TRUE)
    dist_m <- tidyr::unite(dist_m, "m", sep = paste0(sep, " "), na.rm = TRUE)
    return(cbind(dist, dist_m))
  }

  # define list of patterns and description texts
  lp_dt <- data.frame(pattern_text = c("FEW\\d{3}\\s",
                                       "SCT\\d{3}\\s",
                                       "SCT\\d{3}CB",
                                       "BKN\\d{3}\\s",
                                       "BKN\\d{3}CB"),
                                       #"BKN\\/{3}"),
                      description_text = c("Few (1-2 oktas) at ",
                                           "Scattered (3-4 oktas) at ",
                                           "Scattered (3-4 oktas) cumulonimbus clouds at ",
                                           "Broken (5-7 oktas) at ",
                                           "Broken (5-7 oktas) cumulonimbus clouds at "),
                                           #"Broken clouds at NaN "),
                      stringsAsFactors = FALSE)
  out <- c(1:length(x))
  out[1:length(x)] <- ""
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  # SKC - "No cloud/Sky clear" used worldwide but in
  # North America is used to indicate a human generated report
  fT <- stringr::str_detect(x, pattern = "SKC")
  out[fT] <- paste0(out[fT], "No cloud/Sky clear", sep, " ")
  # CLR - "No clouds below 12,000 ft (3,700 m) (U.S.) or 25,000 ft (7,600 m) (Canada)",
  # used mainly within North America and indicates a station that is at least partly automated
  fT <- stringr::str_detect(x, pattern = "CLR")
  out[fT] <- paste0(out[fT], "No clouds below 12 000 ft (3 700 m) (U.S.) or 25 000 ft (7 600 m) (Canada)", sep, " ")
  # NSC - "No (nil) significant cloud", i.e., none below 5,000 ft (1,500 m) and no TCU or CB.
  # Not used in North America.
  fT <- stringr::str_detect(x, pattern = "NSC")
  out[fT] <- paste0(out[fT], "No (nil) significant cloud", sep, " ")
  # iterate through FEWnnn, SCTnnn, SCTnnnCB, BKNnnn, BKNnnnCB
  for (i in 1:nrow(lp_dt)) {
    fT <- stringr::str_detect(x, pattern = as.character(lp_dt$pattern_text[i]))
    if(sum(fT) > 0) {
      df_dist <- as.data.frame(stringr::str_extract_all(x[fT], pattern = as.character(lp_dt$pattern_text[i]), simplify = TRUE),
                               stringsAsFactors = FALSE)
      ldist <- multi_extracting(df_dist, as.character(lp_dt$pattern_text[i]))
      out[fT] <- paste0(out[fT], as.character(lp_dt$description_text[i]), ldist$ft, " ft (", ldist$m, " m)", sep, " ")
    }
  }
  # OVCnnn
  fT <- stringr::str_detect(x, pattern = "OVC[\\d]+\\s")
  dist <- as.numeric(stringr::str_sub(stringr::str_extract(x[fT], pattern = "OVC[\\d]+\\s"), 4, 6)) * 100
  dist_m <- dist * 0.3048
  out[fT] <- paste0(out[fT], "Overcast (8 oktas, full cloud coverage) at ", dist, " ft (", dist_m, " m)", sep, " ")
  # VV - Clouds cannot be seen because of fog or heavy precipitation, so vertical visibility is given instead.
  fT <- stringr::str_detect(x, pattern = "\\sVV\\s")
  out[fT] <- paste0(out[fT], "Clouds cannot be seen because of fog or heavy precipitation")
  fT <- stringr::str_detect(out, pattern = "(,\\s|;\\s$)")
  out[fT] <- stringr::str_sub(out[fT], 1, (nchar(out[fT]) - 2))
  # remove double spaces
  #out <- stringr::str_replace_all(out, pattern = "\\s\\s", replacement = " ")
  out
}

