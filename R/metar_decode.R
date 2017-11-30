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
#' @return A character vector.
#'
#' @export
#'
#' @examples
#' metar_decode("EPWA 281830Z 18009KT 140V200 9999 SCT037 03/M01 Q1008 NOSIG")
#' metar_decode("CYUL 281800Z 13008KT 30SM BKN240 01/M06 A3005 RMK CI5 SLP180")
#' metar_decode("201711271930 METAR LEMD 271930Z 02002KT CAVOK 04/M03 Q1025 NOSIG= NOSIG=")
#'
metar_decode <- function(x, as_data_frame = TRUE){
  # airport
  ap <- sapply(x, metar_airport, USE.NAMES = FALSE)
  # day of a month
  day.month <- sapply(x, metar_day, USE.NAMES = FALSE)
  # hour
  h <- sapply(x, metar_hour, USE.NAMES = FALSE)
  # time zone
  time.zone <- sapply(x, metar_time_zone, USE.NAMES = FALSE)
  # wind speed
  ws <- sapply(x, metar_speed, USE.NAMES = FALSE)
  # wind direction
  wd <- sapply(x, metar_dir, USE.NAMES = FALSE)
  # temperature
  t <- sapply(x, metar_temp, USE.NAMES = FALSE)
  # pressure
  p <- sapply(x, metar_pressure, USE.NAMES = FALSE)
  # location of a metar station
  lmt <- sapply(ap, metar_location, USE.NAMES = FALSE)
  # check if a date of a metar is available
  if(str_detect(x, pattern = START %R% one_or_more(DGT) %R% " METAR")[1]) {
    td <- str_extract(x, pattern = START %R% one_or_more(DGT) %R% " METAR")
    myear <- as.numeric(str_sub(td, 1, 4))
    mmonth <- as.numeric(str_sub(td, 5, 6))
    mday <- as.numeric(str_sub(td, 7, 8))
    mhour <- as.numeric(str_sub(td, 9, 10))
    mminute <- as.numeric(str_sub(td, 11, 12))
    metar_date <- make_datetime(myear, mmonth, mday, mhour, mminute, tz = "UTC")
    data.frame(Airport = ap, Metar.Date = metar_date, Day.of.month = day.month, Hour = h, Time.zone = time.zone, Wind.speed = ws,
               Wind.direction = wd, Temperature = t, Pressure = p,
               Longitude = unlist(lmt[1,]), Latitude = unlist(lmt[2,]), Elevation = unlist(lmt[3,]),
               Decode.Date = Sys.time())
  } else {
    data.frame(Airport = ap, Day.of.month = day.month, Hour = h, Time.zone = time.zone, Wind.speed = ws,
               Wind.direction = wd, Temperature = t, Pressure = p,
               Longitude = unlist(lmt[1,]), Latitude = unlist(lmt[2,]), Elevation = unlist(lmt[3,]),
               Decode.Date = Sys.time())
  }
}
