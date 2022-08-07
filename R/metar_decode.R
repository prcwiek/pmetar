#' Decode METAR report.
#'
#' Extract and parse information from a single METAR weather report or several reports.
#'
#' Decoded METAR weather report consists of:\cr
#' \itemize{
#' \item Remark: Don't use for flight planning or navigation!
#' or Incorrect METAR report! Please check the column Original_METAR.
#' \item Airport ICAO
#' \item Day of Month
#' \item Hour (HH:mm)
#' \item Time zone
#' \item Wind speed
#' \item Wind speed unit (m/s or kn)
#' \item Gust
#' \item Gust unit (m/s or kn)
#' \item Wind shear
#' \item Wind direction (degrees)
#' \item Temperature (Celsius degrees)
#' \item Dew point (Celsius degrees)
#' \item Pressure (hPa)
#' \item Pressure unit (hPa or mmHg)
#' \item Visibility
#' \item Visibility unit (m or miles)
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
#' @param x character vector; a single METAR weather report or\cr
#' historical METAR weather reports.
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
#' @return a tibble with decoded METAR weather report or reports.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' metar_decode("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_decode("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180",
#' altimeter = TRUE, metric = FALSE)
#' metar_decode("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025")
#' metar_decode("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005", altimeter = TRUE)
#' metar_decode("CYWG 172000Z 30015G25KT 3/4SM R36/4000FT/D -SN M05/M08 A2992")
#' metar_decode("202103251800 METAR COR NFTL 251800Z 00000KT SCT017TCU BKN290 25/25 Q1014")
#'
metar_decode <- function(x, metric = TRUE, altimeter = FALSE,
                         numeric_only = FALSE, check = TRUE,
                         sep = ";"){
  # Check sep values
  if (!stringr::str_detect(sep, pattern = "(^;$|^,$)")) {
    stop("pmetar package error: Invalid sep value! It must be comma or semicolon!")
  }
  # check syntax
  if (check) {
    icorrect <- metar_is_correct(x)
    ncorrect <- metar_is_correct(x)
  } else {
    icorrect <- rep(TRUE, length(x))
    ncorrect <- seq(1, length(x), 1)
  }
  
  tryCatch(
    expr = {
      if(sum(stringr::str_detect(x[icorrect], pattern = "^[\\d]+ METAR")) > 0) {
        metar_date <- rep("", length(x))
        metar_date[icorrect] <- stringr::str_extract(x[icorrect], pattern = "^[\\d]+ METAR")
        myear <- as.numeric(stringr::str_sub(metar_date[icorrect], 1, 4))
        mmonth <- as.numeric(stringr::str_sub(metar_date[icorrect], 5, 6))
        mday <- as.numeric(stringr::str_sub(metar_date[icorrect], 7, 8))
        mhour <- as.numeric(stringr::str_sub(metar_date[icorrect], 9, 10))
        mminute <- as.numeric(stringr::str_sub(metar_date[icorrect], 11, 12))
        metar_date[icorrect] <- as.character(lubridate::make_datetime(myear, mmonth, mday, mhour, mminute, tz = "UTC"))
        metar_date[!icorrect] <- NA
      } else {
        metar_date <- NA
      }
      # create an empty template
      out <- dplyr::tibble(#y = x,
                           Remark = "Don't use for flight planning or navigation!",
                           Airport_ICAO = NA,
                           METAR_Date = NA,
                           Day_of_Month = NA,
                           Hour = NA,
                           Time_zone = NA,
                           Wind_speed = NA,
                           Wind_speed_unit = ifelse(metric, "m/s", "kn"),
                           Gust = NA,
                           Gust_unit = ifelse(metric, "m/s", "kn"),
                           Wind_shear = NA,
                           Wind_direction = NA,
                           Temperature = NA,
                           Dew_point = NA,
                           Pressure = NA,
                           Pressure_unit = ifelse(!altimeter, "hPa", "mmHg"),
                           Visibility = NA,
                           Visibility_unit = ifelse(metric, "m", "mile"),
                           Cloud_coverage = NA,
                           Weather_information = NA,
                           Runway_visibility = NA,
                           Airport_Name = NA,
                           Airport_IATA = NA,
                           Longitude = NA,
                           Latitude = NA,
                           Elevation = NA,
                           Decode_Date = NA,
                           Original_METAR = x)
      outx <- out[ncorrect, ] 
      outx <- outx %>%
        dplyr::mutate(Remark = "Don't use for flight planning or navigation!",
                      Airport_ICAO = metar_airport(outx$Original_METAR),
                      METAR_Date = metar_date[icorrect],
                      Day_of_Month = metar_day(outx$Original_METAR),
                      Hour = metar_hour(outx$Original_METAR),
                      Time_zone = metar_time_zone(outx$Original_METAR),
                      Wind_speed = metar_speed(outx$Original_METAR, metric = metric, check = FALSE),
                      Wind_speed_unit = ifelse(metric, "m/s", "kn"),
                      Gust = metar_gust(outx$Original_METAR, metric = metric),
                      Gust_unit = ifelse(metric, "m/s", "kn"),
                      Wind_shear = metar_windshear(outx$Original_METAR, metric = metric),
                      Wind_direction = metar_dir(outx$Original_METAR, numeric_only = numeric_only),
                      Temperature = metar_temp(outx$Original_METAR),
                      Dew_point = metar_dew_point(outx$Original_METAR),
                      Pressure = metar_pressure(outx$Original_METAR, altimeter = altimeter),
                      Pressure_unit = ifelse(!altimeter, "hPa", "mmHg"),
                      Visibility = metar_visibility(outx$Original_METAR, metric = metric),
                      Visibility_unit = ifelse(metric, "m", "mile"),
                      Cloud_coverage = metar_cloud_coverage(outx$Original_METAR, sep = sep),
                      Weather_information = metar_wx_codes(outx$Original_METAR, sep = sep),
                      Runway_visibility = metar_rwy_visibility(outx$Original_METAR, metric = metric, sep = sep))
      apl <- metar_location(outx$Airport_ICAO)
      outx <- outx %>%
        dplyr::mutate(Airport_Name = apl$Airport_Name,
                      Airport_IATA = apl$IATA_Code,
                      Longitude = apl$Longitude,
                      Latitude = apl$Latitude,
                      Elevation = apl$Elevation,
                      Decode_Date = as.character(Sys.time()))
      out[ncorrect,] <- outx
      out$Remark[!ncorrect] <- "Incorrect METAR report! Please check the column Original_METAR."
      out
    },
    error = function(e){
      stop("pmetar package error: Incorrect METAR weather report found and cannot be 
           decoded!\n", call. = FALSE)
    }
  )
}
