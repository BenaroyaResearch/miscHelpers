#' Normalize a numeric vector to 0-1 range
#' @param x numeric vector to be normalized
#' @export
#' @usage \code{range01(x)}
range01 <- function(x)((x-min(x))/(max(x)-min(x)))
