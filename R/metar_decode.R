#' Decode METAR report.
#'
#' Function extract information from METAR weather report. Depending of an input vector format
#' a returned data frame consists of different values.
#'
#'  For a current METAR report:\cr
#' - Airport\cr
#' - Day.of.month\cr
#' - Hour (HH:mm)\cr
#' - Time.zone\cr
#' - Wind.speed (m/s)\cr
#' - Wind.direction (degrees)\cr
#' - Temperature (degrees Celsius)\cr
#' - Pressure (hPa)\cr
#' - Longitude\cr
#' - Latitude\cr
#' - Elevation\cr
#' - Decode.Date\cr\cr
#'
#' For a historical METAR report:\cr
#' - Airport\cr
#' - Metar.Date\cr
#' - Day.of.month\cr
#' - Hour (HH:mm)\cr
#' - Time.zone\cr
#' - Wind.speed (m/s)\cr
#' - Wind.direction (degrees)\cr
#' - Temperature (degrees Celsius)\cr
#' - Pressure (hPa)\cr
#' - Longitude\cr
#' - Latitude\cr
#' - Elevation\cr
#' - Decode.Date\cr
#'
#' @param x Input character vector
#'
#' @return A character vector or a tibble.
#'
#' @export
#'
#' @examples
#' metar_decode("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_decode("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_decode("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_decode <- function(x, as_data_frame = TRUE){
  if(str_detect(x, pattern = "^[\\d]+ METAR")[1]) {
    td <- c(1:length(x))
    td <- str_extract(x, pattern = "^[\\d]+ METAR")
    myear <- as.numeric(str_sub(td, 1, 4))
    mmonth <- as.numeric(str_sub(td, 5, 6))
    mday <- as.numeric(str_sub(td, 7, 8))
    mhour <- as.numeric(str_sub(td, 9, 10))
    mminute <- as.numeric(str_sub(td, 11, 12))
    metar_date <- make_datetime(myear, mmonth, mday, mhour, mminute, tz = "UTC")
    out <- tibble(x)
    out <- out %>%
      mutate(Airport = metar_airport(out$x)) %>%
      mutate(Metar.Date = metar_date) %>%
      mutate(Day.of.month = metar_day(out$x)) %>%
      mutate(Hour = metar_hour(out$x)) %>%
      mutate(Time.zone = metar_time_zone(out$x)) %>%
      mutate(Wind.speed = metar_speed(out$x)) %>%
      mutate(Wind.direction = metar_dir(out$x)) %>%
      mutate(Temperature = metar_temp(out$x)) %>%
      mutate(Pressure = metar_pressure(out$x))
    apl <- metar_location(out$Airport)
    out <- out %>%
      mutate(Longitude = apl$longitude) %>%
      mutate(Latitude = apl$latitude) %>%
      mutate(Elevation = apl$elevation) %>%
      mutate(Decode.Date = Sys.time()) %>%
      select(-x)
  } else {
    out <- tibble(x)
    out <- out %>%
      mutate(Airport = metar_airport(out$x)) %>%
      mutate(Day.of.month = metar_day(out$x)) %>%
      mutate(Hour = metar_hour(out$x)) %>%
      mutate(Time.zone = metar_time_zone(out$x)) %>%
      mutate(Wind.speed = metar_speed(out$x)) %>%
      mutate(Wind.direction = metar_dir(out$x)) %>%
      mutate(Temperature = metar_temp(out$x)) %>%
      mutate(Pressure = metar_pressure(out$x))
    apl <- metar_location(out$Airport)
    out <- out %>%
      mutate(Longitude = apl$longitude) %>%
      mutate(Latitude = apl$latitude) %>%
      mutate(Elevation = apl$elevation) %>%
      mutate(Decode.Date = Sys.time()) %>%
      select(-x)
  }
  out
}
