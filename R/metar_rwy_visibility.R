#' Get runway(s) visibility.
#'
#' Function extracts runway(s) visibility value(s) from METAR weather report.
#'
#' @param x Input character vector
#' @param metric logical; if TRUE, the default value, runway(s) visibility is returned in meters,
#' if FALSE then in feet.
#' @param sep character; comma or semicolon, used for separating decoded elements of weather
#' conditions information.
#' 
#' @return A numeric vector. A visibility in m/s or feet.
#'
#' @export
#'
#' @examples
#'
#' metar_rwy_visibility("EBBR 040220Z VRB01KT 0150 R25L/1200N R02/P1500 07/06 Q1017")
#' metar_rwy_visibility("EBBR 040220Z VRB01KT 0150 R25R/0600FT R02/P1500 07/06 Q1017")
#' metar_rwy_visibility("EDDF 220520Z 26003KT 0500 R25R/0400N R18/0650V1100N FZFG", sep = ",")
#' metar_rwy_visibility("CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN M05/M08 A2992")
#' metar_rwy_visibility("EBBR 040220Z VRB01KT 0150 R25L/1200N R26R/1000 R36/4000FT/D -SN")
#'
metar_rwy_visibility <- function(x, metric = TRUE, sep = ";") {
  # check if x is a data frame
  if(is.data.frame(x)){
    stop("pmetar package error: Invalid input format! Argument is not an atomic vector.", call. = FALSE)
  }
  # Check sep values
  if (!stringr::str_detect(sep, pattern = "(^;$|^,$)")) {
    stop("pmetar package error: Invalid sep value! It must be comma or semicolon!")
  }
  # define conversion coefficients
  if(metric){
    cfm <- 1
    cfi <- 0.3048
    tdist <- "meters"
  } else {
    cfm <- 1/0.3048
    cfi <- 1
    tdist <- "ft"
  }
  outvis <- c(1:length(x))
  outvis[1:length(x)] <- ""
  # Remove part after RMK
  x <- stringr::str_split_fixed(x, pattern = "RMK", n = 2)[,1]
  # Remove part after TEMPO
  x <- stringr::str_split_fixed(x, pattern = "TEMPO", n = 2)[,1]
  rvr_code_resolve <- function(rvr) {
    if(length(rvr) == 0) {
      return("")
    }
    out_rvr <- rvr
    for (i in 1:length(rvr)) {
      item_found <- FALSE
      # -------------------------------------------------------------------------
      # cases with feet
      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}V\\d{4}FT") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is variable from ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                                        pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}"),
                                                                   pattern = "\\d{4}$")) * cfi, 2),
                             " to ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                                        pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}V\\d{4}"),
                                                                   pattern = "\\d{4}$")) * cfi, 2),

                             " ", tdist)
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/N")) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/N"),
                                                          pattern = "\\d{4}")) * cfi, 2),
                             " ", tdist, " with static trend")
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/U") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/U"),
                                                          pattern = "\\d{4}")) * cfi, 2),
                             " ", tdist, " with upward trend")
        item_found <- TRUE
      }
      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/D") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT\\/D"),
                                                          pattern = "\\d{4}")) * cfi, 2),
                             " ", tdist, " with downward trend")
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)P\\d{4}FT") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is greater than ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                                        pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)P\\d{4}"),
                                                                   pattern = "\\d{4}")) * cfi, 2),
                             " ", tdist)
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}FT"),
                                                          pattern = "\\d{4}")) * cfi, 2),
                             " ", tdist)
        item_found <- TRUE
      }

      # -------------------------------------------------------------------------
      # cases with meters
      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}V\\d{4}(\\s|N)") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is from ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                                        pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}"),
                                                                   pattern = "\\d{4}$")) * cfm, 2),
                             " to ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                                        pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}V\\d{4}"),
                                                                   pattern = "\\d{4}$")) * cfm, 2),

                             " ", tdist)
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}N") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}N"),
                                                          pattern = "\\d{4}")) * cfm, 2),
                             " ", tdist, " with static trend")
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}U") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}U"),
                                                          pattern = "\\d{4}")) * cfm, 2),
                             " ", tdist, " with upward trend")
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}D") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}D"),
                                                          pattern = "\\d{4}")) * cfm, 2),
                             " ", tdist, " with downward trend")
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)P\\d{4}") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is greater than ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)P\\d{4}"),
                                                          pattern = "\\d{4}")) * cfm, 2),
                             " ", tdist)
        item_found <- TRUE
      }

      if(stringr::str_detect(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}") & !item_found) {
        out_rvr[i] <- paste0("Runway visual range for runway ",
                             stringr::str_sub(stringr::str_extract(rvr[i], pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)"), 1, -2),
                             " is ",
                             round(as.numeric(stringr::str_extract(stringr::str_extract(rvr[i],
                                                                      pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)\\d{4}"),
                                                          pattern = "\\d{4}")) * cfm, 2),
                             " ", tdist)
      }


    }
    out_rvr
  }

  rvr_code_extract <- function(rvre, rvr_case = 0) {
    rvre <- as.data.frame(rvre, stringsAsFactors = FALSE)
    if (nrow(rvre) == 1) {
      rvr_extracted <- t(apply(rvre, 1, rvr_code_resolve))
    } else if (nrow(rvre) != 0) {
      rvr_extracted <- apply(rvre, 2, rvr_code_resolve)
    } else {
      rvr_extracted = matrix(data = "", nrow = 1, ncol = 1)
    }

    out_rce <- c(1:nrow(rvr_extracted))
    for (j in 1:nrow(rvr_extracted)) {
      out_rce[j] <- paste(rvr_extracted[j,], collapse = paste0(sep, " "))
      out_rce[j] <- stringr::str_replace(as.character(out_rce[j]), pattern = "([;\\s]+$|[,\\s]+$)", replacement =  "")
    }

    out_rce
  }

  outvis_temp <- stringr::str_extract_all(x,
                                          pattern = "(R|RWY)\\d{2}([A-Z]\\/|\\/)(\\d{4}|[P]\\d{4}|\\d{4}V\\d{4})(\\s|(N|U|D)|FT\\s|(FT\\/N|FT\\/U|FT\\/D))",
                                          simplify = TRUE)
  if (ncol(outvis_temp) > 0) {
    outvis <- rvr_code_extract(outvis_temp)
  }
  outvis
}

