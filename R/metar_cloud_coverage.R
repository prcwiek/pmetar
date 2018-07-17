#' Extract cloud coverage information.
#'
#' Function extract cloud coverage information from METAR weather report.
#'
#' @param x Input character vector
#'
#' @return A character vector with cloud coverage information.
#'
#' @export
#'
#' @examples
#' metar_cloud_coverage("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_cloud_coverage("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_cloud_coverage("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_cloud_coverage <- function(x) {
  outcc <- c(1:length(x))
  outcc[1:length(x)] <- ""
  # SKC - "No cloud/Sky clear" used worldwide but in
  # North America is used to indicate a human generated report
  fT <- str_detect(x, pattern = "SKC")
  outcc[fT] <- paste0(outcc[fT], "No cloud/Sky clear, ")
  # CLR - "No clouds below 12,000 ft (3,700 m) (U.S.) or 25,000 ft (7,600 m) (Canada)",
  # used mainly within North America and indicates a station that is at least partly automated
  fT <- str_detect(x, pattern = "CLR")
  outcc[fT] <- paste0(outcc[fT], "No clouds below 12,000 ft (3,700 m) (U.S.) or 25,000 ft (7,600 m) (Canada), ")
  # NSC - "No (nil) significant cloud", i.e., none below 5,000 ft (1,500 m) and no TCU or CB.
  # Not used in North America.
  fT <- str_detect(x, pattern = "NSC")
  outcc[fT] <- paste0(outcc[fT], "No (nil) significant cloud, ")
  # FEW
  fT <- str_detect(x, pattern = "FEW[\\d]+\\s")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "FEW[\\d]+\\s"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Few (1–2 oktas) at ", dist, " ft, ")
  # SCT
  fT <- str_detect(x, pattern = "SCT[\\d]+\\s")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "SCT[\\d]+\\s"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Scattered (3–4 oktas) at ", dist, " ft, ")
  # SCTnnnCB
  fT <- str_detect(x, pattern = "SCT[\\d]+CB")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "SCT[\\d]+CB"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Scattered (3–4 oktas) cumulonimbus clouds at ", dist, " ft, ")
  # BKN
  fT <- str_detect(x, pattern = "BKN[\\d]+\\s")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "BKN[\\d]+\\s"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Broken (5–7 oktas) at ", dist, " ft, ")
  # BKNnnnCB
  fT <- str_detect(x, pattern = "BKN[\\d]+CB")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "BKN[\\d]+CB"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Broken (5–7 oktas) cumulonimbus clouds at ", dist, " ft, ")
  # OVC
  fT <- str_detect(x, pattern = "OVC[\\d]+\\s")
  dist <- as.numeric(str_sub(str_extract(x[fT], pattern = "OVC[\\d]+\\s"), 4, 6)) * 100
  outcc[fT] <- paste0(outcc[fT], "Overcast (8 oktas, full cloud coverage) at  ", dist, " ft, ")
  # VV - Clouds cannot be seen because of fog or heavy precipitation, so vertical visibility is given instead.
  fT <- str_detect(x, pattern = "\\sVV\\s")
  outcc[fT] <- paste0(outcc[fT], "Clouds cannot be seen because of fog or heavy precipitation")
  fT <- str_detect(outcc, pattern = ", $")
  outcc[fT] <- str_sub(outcc[fT], 1, (nchar(outcc[fT]) - 2))
  outcc[outcc == ""] <- NA
  outcc
}