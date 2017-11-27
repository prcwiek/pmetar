#' metar_hour
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_hour <- function(x){
  mt <- str_extract(x, pattern = one_or_more(WRD) %R% SPC %R% one_or_more(DGT) %R% ANY_CHAR %R% SPC)
  paste(str_sub(mt, 8, 9), str_sub(mt, 10, 11), sep = ":")
}
