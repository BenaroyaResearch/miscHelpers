#' Calculate the number of non-NA elements in an object
#'
#' Calculate the number of non-NA elements in an object. Can be useful for determining the sparseness of matrices and data frames, or their component rows or columns.
#' @param x object to be tested for non-NA values. Can be a data.frame, matrix, vector, list. For lists, it looks at the list elements themselves, not within those elements. For other object types, results may not be as expected.
#' @export
#' @return integer, the number of non-NA values in \code{x}
#' @usage count_not_NAs(x)
count_NAs <- function(x) sum(!is.na(x))
