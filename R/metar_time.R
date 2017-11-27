#' metar_time
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_time <- function(x) {
  pattern_t <- one_or_more(DGT) %R% ANY_CHAR %R% SPC
  mt <- str_extract(x, pattern = pattern_t)
  mt <- paste(str_sub(mt, 3, 4), str_sub(mt, 5, 6), sep = ":")
  mt <- paste(str_sub(Sys.time(), 1, 10), mt)
  as.POSIXct(strptime(mt, tz = "UTC", format = "%Y-%m-%d %H:%M"))
}
