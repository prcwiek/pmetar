#' metar_dir
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_dir <- function(x){
  if(str_detect(x, pattern = one_or_more(DGT) %R% "G" %R% one_or_more(DGT) %R% "KT")){
    dirw <- str_extract(x, pattern = one_or_more(DGT) %R% "KT")
    as.numeric(str_sub(dirw, 1, 3))
  } else if(str_detect(x, pattern = "VRB" %R% one_or_more(DGT) %R% "KT")){
    dirw <- NA
    dirw
  } else {
    dirw <- str_extract(x, pattern = one_or_more(DGT) %R% "KT")
    as.numeric(str_sub(dirw, 1, 3))
  }
}
