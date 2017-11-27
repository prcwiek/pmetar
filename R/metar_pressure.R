#' metar_pressure
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_pressure <- function(x){
  pattern_pressure <- "Q" %R% one_or_more(DGT)
  if(str_detect(x, pattern = pattern_pressure)){
    pressure <- str_extract(x, pattern = pattern_pressure)
    as.numeric(str_sub(pressure, 2, nchar(pressure)))
  } else {
    pattern_pressure <- "A" %R% one_or_more(DGT)
    pressure <- str_extract(x, pattern = pattern_pressure)
    as.numeric(str_sub(pressure, 2, nchar(pressure))) * 0.3386389
  }
}
