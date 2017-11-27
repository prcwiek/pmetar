#' metar_time_zone
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_time_zone <- function(x){
  pattern_t <- one_or_more(DGT) %R% ANY_CHAR %R% SPC
  mt <- str_extract(x, pattern = pattern_t)
  if(str_sub(mt, 7, 7) == "Z") "UTC" else "local time"
}
