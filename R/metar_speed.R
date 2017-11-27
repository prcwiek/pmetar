#' metar_speed
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_speed <- function(x){
  if(str_detect(x, pattern = one_or_more(DGT) %R% "MPS")) {
    speed <- str_extract(x, pattern = one_or_more(DGT) %R% "MPS")
    as.numeric(str_sub(speed, 1, 2))
  } else if(str_detect(x, pattern = one_or_more(DGT) %R% "KT")) {
    speed <- str_extract(x, pattern = one_or_more(DGT) %R% "KT")
    as.numeric(str_sub(speed, 1, 2)) * 0.514444
  }
}
