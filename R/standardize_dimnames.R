#' Standardize the column and/or row names of a matrix or data frame to standard syntax
#'
#' Standardize column and/or row names of a matrix or data frame to a standard syntax. This is a wrapper for
#' \code{standardize_names}, to allow it to be used on data frames and matrices, rather than on their
#' names. All letters are converted to lowercase, special characters are converted to underscores,
#' and multiple and trailing (but not leading) special characters are removed. By default, elements are
#' deduplicated by appending "_1" and so on.
#' @param x data frame or matrix with column and/or row names to be standardized
#' @param cols,rows logical, whether to standardize the column and row names, respectively. By default, column names are standardized, but row names are not.
#' @param dedup logical, whether to deduplicate the vector elements. If TRUE, elements that are identical after standardization are deduplicated by appending "_1", "_2", etc. Defaults to TRUE.
#' @import stringr
#' @export
#' @return an object of the same type and dimension as \code{x}
#' @usage standardize_dimnames(x, cols=TRUE, rows=FALSE, dedup=TRUE)
standardize_dimnames <- function(x, cols=TRUE, rows=FALSE, dedup=TRUE) {
  if (!is.data.frame(x) & !is.matrix(x))
    stop("Input object type not recognized. This function is for use on data frames and matrices.")
  if (cols)
    colnames(x) <- standardize_names(colnames(x), dedup=dedup)
  if (rows)
    rownames(x) <- standardize_names(rownames(x), dedup=dedup)
  x
}
