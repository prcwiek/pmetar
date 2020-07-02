#' Decode METAR report.
#'
#' Extracts information from a single METAR weather report or several reports.
#'
#'  Decoded METAR weather report consists of:\cr
#' \itemize{
#' \item Airport ICAO\cr
#' \item Day of Month\cr
#' \item Hour (HH:mm)\cr
#' \item Time zone\cr
#' \item Wind speed \cr
#' \item Wind speed unit (m/s or kn)\cr
#' \item Gust \cr
#' \item Gust unit (m/s or kn) \cr
#' \item Wind shear\cr
#' \item Wind direction (degrees)\cr
#' \item Temperature (Celsius degrees)\cr
#' \item Dew point (Celsius degrees)\cr
#' \item Pressure (hPa)\cr
#' \item Visibility\cr
#' \item Visibility unit (m or miles)\cr
#' \item Cloud coverage\cr
#' \item Weather information from METAR WX codes\cr
#' \item Airport Name\cr
#' \item Longitude\cr
#' \item Latitude\cr
#' \item Elevation\cr
#' \item Decode Date\cr
#' \item Original METAR text\cr
#' \item Source of information\cr
#' \item Licence\cr\cr
#' }
#'
#' @param x character vector; a single METAR weather report or historical METAR weather reports.
#' @param metric logical; if TRUE the metric units will be used, if FALSE, the imperial units.
#'
#' @return A tibble with decoded METAR weather report or reports.
#'
#' @importFrom magrittr %>%
#'
#' @export
#'
#' @examples
#' metar_decode("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_decode("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_decode("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_decode <- function(x, metric = TRUE){
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
        out <- dplyr::tibble(x)
        out <- out %>%
          dplyr::mutate(Airport_ICAO = metar_airport(out$x)) %>%
          dplyr::mutate(Metar_Date = metar_date) %>%
          dplyr::mutate(Day_of_Month = metar_day(out$x)) %>%
          dplyr::mutate(Hour = metar_hour(out$x)) %>%
          dplyr::mutate(Time_zone = metar_time_zone(out$x)) %>%
          dplyr::mutate(Wind_speed = metar_speed(out$x, metric)) %>%
          dplyr::mutate(Wind_speed_unit = ifelse(metric, "m/s", "kn")) %>%
          dplyr::mutate(Gust = metar_gust(out$x, metric)) %>%
          dplyr::mutate(Gust_unit = ifelse(metric, "m/s", "kn")) %>%
          dplyr::mutate(Wind_shear = metar_windshear(out$x, metric)) %>%
          dplyr::mutate(Wind_direction = metar_dir(out$x)) %>%
          dplyr::mutate(Temperature = metar_temp(out$x)) %>%
          dplyr::mutate(Dew_point = metar_dew_point(out$x)) %>%
          dplyr::mutate(Pressure = metar_pressure(out$x)) %>%
          dplyr::mutate(Visibility = metar_visibility(out$x, metric)) %>%
          dplyr::mutate(Visibility_unit = ifelse(metric, "m", "mile")) %>%
          dplyr::mutate(Cloud_coverage = metar_cloud_coverage(out$x)) %>%
          dplyr::mutate(Weather_information = metar_wx_codes(out$x))
        apl <- metar_location(out$Airport_ICAO)
        out <- out %>%
          dplyr::mutate(Airport_Name = apl$Airport_Name) %>%
          dplyr::mutate(Airport_IATA = apl$IATA_Code) %>%
          dplyr::mutate(Longitude = apl$Longitude) %>%
          dplyr::mutate(Latitude = apl$Latitude) %>%
          dplyr::mutate(Elevation = apl$Elevation) %>%
          dplyr::mutate(Decode_Date = Sys.time()) %>%
          dplyr::mutate(Original_METAR = out$x) %>%
          dplyr::mutate(Source = "mesonet.agron.iastate.edu/AWOS or www.ogimet.com") %>%
          dplyr::mutate(Licence = "ANNEX 1 TO WMO RESOLUTION 40 (Cg-XII) http://www.nws.noaa.gov/im/wmor40a1.htm") %>%
          dplyr::select(-x)
      } else {
        out <- dplyr::tibble(x)
        out <- out %>%
          dplyr::mutate(Airport_ICAO = metar_airport(out$x)) %>%
          dplyr::mutate(Metar_Date = NA) %>%
          dplyr::mutate(Day_of_Month = metar_day(out$x)) %>%
          dplyr::mutate(Hour = metar_hour(out$x)) %>%
          dplyr::mutate(Time_zone = metar_time_zone(out$x)) %>%
          dplyr::mutate(Wind_speed = metar_speed(out$x, metric)) %>%
          dplyr::mutate(Wind_speed_unit = ifelse(metric, "m/s", "kn")) %>%
          dplyr::mutate(Gust = metar_gust(out$x, metric)) %>%
          dplyr::mutate(Gust_unit = ifelse(metric, "m/s", "kn")) %>%
          dplyr::mutate(Wind_shear = metar_windshear(out$x, metric)) %>%
          dplyr::mutate(Wind_direction = metar_dir(out$x)) %>%
          dplyr::mutate(Temperature = metar_temp(out$x)) %>%
          dplyr::mutate(Dew_point = metar_dew_point(out$x)) %>%
          dplyr::mutate(Pressure = metar_pressure(out$x)) %>%
          dplyr::mutate(Visibility = metar_visibility(out$x, metric)) %>%
          dplyr::mutate(Visibility_unit = ifelse(metric, "m", "mile")) %>%
          dplyr::mutate(Cloud_coverage = metar_cloud_coverage(out$x)) %>%
          dplyr::mutate(Weather_information = metar_wx_codes(out$x))
        apl <- metar_location(out$Airport_ICAO)
        out <- out %>%
          dplyr::mutate(Airport_Name = apl$Airport_Name) %>%
          dplyr::mutate(Airport_IATA = apl$IATA_Code) %>%
          dplyr::mutate(Longitude = apl$Longitude) %>%
          dplyr::mutate(Latitude = apl$Latitude) %>%
          dplyr::mutate(Elevation = apl$Elevation) %>%
          dplyr::mutate(Decode_Date = Sys.time()) %>%
          dplyr::mutate(Original_METAR = out$x) %>%
          dplyr::mutate(Source = "www.aviationweather.gov/metar") %>%
          dplyr::mutate(Licence = "ANNEX 1 TO WMO RESOLUTION 40 (Cg-XII) http://www.nws.noaa.gov/im/wmor40a1.htm") %>%
          dplyr::select(-x)
      }
      out
    },
    error = function(e){
      cat("It is not a METAR weather report!\n")
      return(NA)
    }
  )
}
