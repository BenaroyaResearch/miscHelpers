#' Trim any rows and/or columns containing all NAs from a matrix or data frame.
#'
#' Trim any rows and/or columns containing all NAs from a matrix or data frame. Any rows or columns
#' composed entirely of NAs are removed.This is used primarily after reading in a spreadsheet, when excess
#' columns and/or rows may be included due to quirks in the file reading process. It can also remove cases
#' and/or variables with no data.
#' @param x data frame or matrix with rows and/or columns to be removed
#' @param cols,rows logical, whether to trim columns and rows, respectively. By default, both columns and rows are trimmed.
#' @export
#' @return an object of the same type as \code{x}, potentially with fewer rows and/or columns
#' @usage \code{remove_all_NA_rowcols(x, cols=TRUE, rows=TRUE)}
remove_all_NA_rowcols <- function(x, cols=TRUE, rows=TRUE) {
  if (!is.data.frame(x) & !is.matrix(x))
    stop("Input object type not recognized. This function is for use on data frames and matrices.")
  if (cols) {
    keep_cols <-
      apply(x, MARGIN=2, function(x) {sum(!is.na(x))}) > 0
    colnames.orig <- colnames(x)[keep_cols] # necessary to avoid deduplicating column names
    x <- x[,keep_cols]
    colnames(x) <- colnames.orig # necessary to avoid deduplicating column names
  }
  
  if (rows) {
    keep_rows <-
      apply(x, MARGIN=1, function(x) {sum(!is.na(x))}) > 0
    x <- x[keep_rows,]
  }
  
  x
}
