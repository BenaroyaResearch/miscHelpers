#' Calculate the coefficient of variation
#'
#' Calculate the coefficient of variation a vector of numbers, as \code{SD(x) / mean(x)}.
#' This function does NOT calculate percent CV - that requires multiplying the returned
#' values by 100.
#' @param x numeric vector or matrix
#' @param na.rm logical. Should missing values be removed?
#' @export
#' @return a single numeric value
#' @usage \code{cv(x, na.rm=FALSE)}
cv <- function(x, na.rm=FALSE) {
  sd(x, na.rm=na.rm) / mean(x, na.rm=na.rm)
}
