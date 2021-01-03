#' Decode METAR report.
#'
#' Extract and parse information from a single METAR weather report or several reports.
#'
#' Decoded METAR weather report consists of:\cr
#' \itemize{
#' \item Remark: Don't use for flight planning or navigation!
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
#' @param altimeter logical; if FLASE pressures returned in hPa, if TRUE in mmHg
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
#'
metar_decode <- function(x, metric = TRUE, altimeter = FALSE){
  tryCatch(
    expr = {
      if(stringr::str_detect(x, pattern = "^[\\d]+ METAR")[1]) {
        td <- c(1:length(x))
        td <- stringr::str_extract(x, pattern = "^[\\d]+ METAR")
        myear <- as.numeric(stringr::str_sub(td, 1, 4))
        mmonth <- as.numeric(stringr::str_sub(td, 5, 6))
        mday <- as.numeric(stringr::str_sub(td, 7, 8))
        mhour <- as.numeric(stringr::str_sub(td, 9, 10))
        mminute <- as.numeric(stringr::str_sub(td, 11, 12))
        metar_date <- lubridate::make_datetime(myear, mmonth, mday, mhour, mminute, tz = "UTC")
      } else {
        metar_date <- NA
      }
      out <- dplyr::tibble(x)
      out <- out %>%
        dplyr::mutate(Remark = "Don't use for flight planning or navigation!",
                      Airport_ICAO = metar_airport(out$x),
                      METAR_Date = metar_date,
                      Day_of_Month = metar_day(out$x),
                      Hour = metar_hour(out$x),
                      Time_zone = metar_time_zone(out$x),
                      Wind_speed = metar_speed(out$x, metric = metric),
                      Wind_speed_unit = ifelse(metric, "m/s", "kn"),
                      Gust = metar_gust(out$x, metric = metric),
                      Gust_unit = ifelse(metric, "m/s", "kn"),
                      Wind_shear = metar_windshear(out$x, metric = metric),
                      Wind_direction = metar_dir(out$x),
                      Temperature = metar_temp(out$x),
                      Dew_point = metar_dew_point(out$x),
                      Pressure = metar_pressure(out$x, altimeter = altimeter),
                      Pressure_unit = ifelse(!altimeter, "hPa", "mmHg"),
                      Visibility = metar_visibility(out$x, metric = metric),
                      Visibility_unit = ifelse(metric, "m", "mile"),
                      Cloud_coverage = metar_cloud_coverage(out$x),
                      Weather_information = metar_wx_codes(out$x),
                      Runway_visibility = metar_rwy_visibility(out$x, metric = metric))
      apl <- metar_location(out$Airport_ICAO)
      out <- out %>%
        dplyr::mutate(Airport_Name = apl$Airport_Name,
                      Airport_IATA = apl$IATA_Code,
                      Longitude = apl$Longitude,
                      Latitude = apl$Latitude,
                      Elevation = apl$Elevation,
                      Decode_Date = Sys.time(),
                      Original_METAR = out$x) %>%
        dplyr::select(-x)
      out
    },
    error = function(e){
      stop("pmetar package error: It is not a METAR weather report!\n", call. = FALSE)
    }
  )
}
