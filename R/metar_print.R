#' Decode and print METAR report
#'
#' Extract, parse and print information from a single METAR weather report.
#'
#' Function prints below decoded METAR weather report elements:\cr
#' \itemize{
#' \item Remark: Don't use for flight planning or navigation!
#' or Incorrect METAR report! Please check the column Original_METAR.
#' \item Airport ICAO
#' \item Day of Month
#' \item Hour (HH:mm)
#' \item Time zone
#' \item Wind speed (m/s or kn)
#' \item Gust (m/s or kn)
#' \item Wind shear
#' \item Wind direction (degrees)
#' \item Temperature (Celsius degrees)
#' \item Dew point (Celsius degrees)
#' \item Pressure (hPa or mmHg)
#' \item Pressure unit (hPa or mmHg)
#' \item Visibility (m or miles)
#' \item Cloud coverage
#' \item Weather conditions information from WX codes
#' \item Runway visibility (m or feet)
#' \item Airport Name
#' \item Longitude
#' \item Latitude
#' \item Elevation
#' \item Decode Date
#' \item Original METAR text
#' }
#'
#' @param x character vector; a single METAR weather report.
#' @param metric logical; if TRUE wind speeds returned in m/s, distances in meters.\cr
#' If FALSE, wind speeds returned in knots and distances in miles.
#' @param altimeter logical; if FLASE pressures returned in hPa, if TRUE in mmHg.
#' @param numeric_only logical; if TRUE only numeric values are returned.
#' @param check logical; if TRUE the syntax of METAR reports will be checked and incorrect
#' reports will be omitted. If FALSE, the incorrect syntax of reports can cause errors and
#' breakdown of decoding. The default value is TRUE.
#' @param sep character; comma or semicolon, used for separating decoded elements of weather
#' conditions information. The default value is ";".
#'
#' @export
#'
#' @examples
#' metar_print("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_print("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180",
#' altimeter = TRUE, metric = FALSE)
#' metar_print("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_print("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005", altimeter = TRUE)
#' metar_print("CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN M05/M08 A2992")
#' metar_print("202103251800 METAR COR NFTL 251800Z 00000KT SCT017TCU BKN290 25/25 Q1014")
#'
metar_print <- function(x, metric = TRUE, altimeter = FALSE,
                        numeric_only = FALSE, check = TRUE,
                        sep = ";"){
  # check if x is a data frame or length of x is not 1
  if (is.data.frame(x) | length(x) != 1){
    stop("pmetar package error: Invalid input format! Argument is not a string.", call. = FALSE)
  }
  # check sep values
  if (!stringr::str_detect(sep, pattern = "(^;$|^,$)")) {
    stop("pmetar package error: Invalid sep value! It must be comma or semicolon!")
  }
  out <- metar_decode(x, metric = metric, altimeter = altimeter,
                      numeric_only = numeric_only, check = check,
                      sep = sep)
  cat("Decoded METAR report\n")
  cat("--------------------\n")
  cat("Remarks            :\t", out$Remark, "\n")
  cat("Airport ICAO code  :\t", out$Airport_ICAO, "\n")
  cat("METAR date         :\t", out$METAR_Date, "\n")
  cat("Day of month       :\t", out$Day_of_Month, "\n")
  cat("Hour               :\t", out$Hour, "\n")
  cat("Time zone          :\t", out$Time_zone, "\n")
  cat("Wind speed         :\t", round(out$Wind_speed, 2), " ", out$Wind_speed_unit, "\n")
  cat("Gust               :\t", round(out$Gust, 2), " ", ifelse(is.na(out$Gust), "", out$Gust_unit), "\n")
  cat("Wind shear         :\t", out$Wind_shear, "\n")
  cat("Wind direction     :\t", out$Wind_direction, "\n")
  cat("Temperature        :\t", out$Temperature, ifelse(is.na(out$Temperature), "", " \u00b0C"), "\n")
  cat("Dew point          :\t", out$Dew_point, ifelse(is.na(out$Dew_point), "", " \u00b0C"), "\n")
  cat("Pressure           :\t", out$Pressure, " ", out$Pressure_unit, "\n")
  cat("Visibility         :\t", out$Visibility, " ",
      ifelse(is.na(out$Visibility), "",
             ifelse(out$Visibility != "Ceiling And Visibility OK", out$Visibility_unit, "")), "\n")
  cat("Cloud coverage     :\t", out$Cloud_coverage, "\n")
  cat("Weather information:\t", out$Weather_information, "\n")
  cat("Runway visibility  :\t", out$Runway_visibility, "\n")
  cat("Airport name       :\t", out$Airport_Name, "\n")
  cat("Airport IATA code  :\t", out$Airport_IATA, "\n")
  cat("Longitude          :\t", out$Longitude, "\u00b0", "\n")
  cat("Latitude           :\t", out$Latitude, "\u00b0", "\n")
  cat(sprintf("Elevation          :\t %.1f m", out$Elevation), "\n")
  cat("Decode date        :\t", out$Decode_Date, "\n")
  cat("Original METAR     :\t", out$Original_METAR, "\n")
}
