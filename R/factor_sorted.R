#' Convert a vector to a factor with levels sorted
#'
#' This function converts a vector of any class to a factor with levels sorted according to specifications. It is a wrapper for \code{factor()} and \code{stringr::str_sort()}. By default, it sorts mixed character/numeric using the "numeric = TRUE" option for \code{str_sort}.
#' @param x vector to convert
#' @param numeric logical, passed to \code{stringr::str_sort}
#' @param ... optional additional arguments passed to \code{stringr::str_sort}
#' @importFrom stringr str_sort
#' @importFrom utils type.convert
#' @export
#' @return vector of the same length as x
factor_sorted <- function(x, numeric = TRUE, ...) {
  if (is.factor(x)) {
    # Try to infer the "real" type of levels
    x <- type.convert(as.character(x), as.is = TRUE)
  }
  
  if (!is.atomic(x)) stop("Input `x` must be an atomic vector")

  lvls <- str_sort(unique(x), numeric = numeric, ...)
  factor(x, levels = lvls)
}
