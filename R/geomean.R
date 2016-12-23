#' Calculate the geometric mean of a numeric vector
#'
#' Calculate the geometric mean of a numeric vector, passing arguments on to \code{mean}.
#' @param x numeric vector. All elements must be positive, or an error is thrown. Geometric mean is defined only where all elements are positive.
#' @param ... optional arguments, passed to \code{mean}. \code{na.rm} is particularly useful.
#' @export
#' @return numeric, the value of the geometric mean
#' @usage \code{geomean(x, ...)}
geomean <- function(x, ...) {
  exp(mean(log(x), ...))
}
