#' metar_day
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_day <- function(x){
  pattern_t <- one_or_more(DGT) %R% ANY_CHAR %R% SPC
  mt <- str_extract(x, pattern = pattern_t)
  str_sub(mt, 1, 2)
}
