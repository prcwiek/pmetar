#' metar_airport
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_airport <- function(x) {
  if(str_detect(x, pattern = START %R% one_or_more(DGT) %R% " METAR")){
    out <- str_extract(x, pattern = "METAR " %R% one_or_more(WRD))
    str_sub(out, nchar(out)-3, nchar(out))
  } else {
    ap <- str_extract(x, pattern = START %R% one_or_more(WRD) %R% SPC)
    str_sub(ap, 1, 4)
  }
}
