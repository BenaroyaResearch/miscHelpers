#' Calculate the standard error of the mean
#'
#' Calculate the standard error of the mean of a vector of numbers
#' @param x numeric vector or matrix
#' @param na.rm logical. Should missing values be removed?
#' @export
#' @return a single numeric value
#' @usage \code{se(x)}
se <- function(x, na.rm=FALSE) {
  sd(x, na.rm=na.rm) / 
    if (!na.rm) sqrt(length(x)) else sqrt(length(x) - sum(is.na(x)))
}
