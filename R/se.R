#' Calculate the standard error of the mean
#'
#' Calculate the standard error of the mean of a vector of numbers, as \code{SD(x) / sqrt(N)}.
#' @param x numeric vector or matrix
#' @param na.rm logical. Should missing values be removed?
#' @export
#' @return a single numeric value
#' @usage se(x, na.rm=FALSE)
se <- function(x, na.rm=FALSE) {
  sd(x, na.rm=na.rm) / 
    if (!na.rm) sqrt(length(x)) else sqrt(length(x) - sum(is.na(x)))
}
