#' metar_temp
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_temp <- function(x){
  pattern_temp <- "M" %R% one_or_more(DGT) %R% "/"
  if(str_detect(x, pattern = pattern_temp)) {
    temp <- str_extract(x, pattern = pattern_temp)
    as.numeric(str_sub(temp, 2, 3)) * -1
  } else {
    pattern_temp <- one_or_more(DGT) %R% "/" # %R% one_or_more(DGT)
    temp <- str_extract(x, pattern = pattern_temp)
    as.numeric(str_sub(temp, 1, 2))
  }
}
