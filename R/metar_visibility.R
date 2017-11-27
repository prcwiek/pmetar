#' metar_visibility
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
metar_visibility <- function(x) {
  pattern_ap <- SPC %R% one_or_more(DGT) %R% SPC
  ap <- str_extract(x, pattern = pattern_ap)
  as.numeric(str_sub(ap, 2, 5))
}
