#' Test a vector (numeric or otherwise) for equivalency across all elements
#'
#' Test a vector (numeric or otherwise) for equivalency across all elements. For numeric values, a tolerance is used to test equivalency. For non-numeric values, \code{unique} is used to determine equivalency.
#' @param x vector to test for equivalency
#' @param tol numeric, the tolerance to use for equivalency of numeric objects. Ignored if \code{x} is not numeric. Defaults to the square-root of the machine precision, typically about 1.5e-8.
#' @export
#' @return logical, either TRUE (all elements are equivalent) or FALSE (some elements are not equivalent)
#' @usage all_equal(x, tol = .Machine$double.eps ^ 0.5)
all_equal <- function(x, tol = .Machine$double.eps ^ 0.5) {
  if (is.numeric(x)) {
    abs(max(x) - min(x)) < tol
  } else {
    length(unique(x)) == 1
  }
}
