#' Coerce textual NAs to true NA values
#'
#' This function is designed to convert textual NAs to true NA values. It can operate on vectors, data
#' frames, data frame like objects (e.g. tibbles), or matrices. It returns an object of the same class.
#' The values that will be coerced to NAs can be provided as a character vector.
#' @param x vector, matrix, or data frame containing the data to be coerced
#' @param NA_values character vector, the values to be coerced to NAs.
#' @param verbose logical, whether to output information on data being coerced; if \code{x} is a data frame, the function will output the names of the columns with data being coerced.
#' @export
#' @return An object of the same type as the input data
#' @usage coerce_NA_text(x, NA_values=c("NA", "na", "N/A", "n/a", "", "."), verbose=FALSE)

coerce_NA_text <- function(x, NA_values=c("NA", "na", "N/A", "n/a", "", "."), verbose=FALSE) {
  if (is.vector(x) | is.matrix(x)) {
    x[x %in% NA_values] <- NA
  } else if (inherits(x, "data.frame")) {
    for (col.tmp in 1:ncol(x))
      if (any(x[[col.tmp]] %in% NA_values)) {
        x[[col.tmp]][x[[col.tmp]] %in% NA_values] <- NA
        if (verbose) print(paste("Coerced textual NAs to true NAs for column", colnames(x)[col.tmp]))
      }
  } else stop("Input data object must be a vector, data frame")
  x
}