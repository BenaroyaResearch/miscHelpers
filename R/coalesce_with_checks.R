#' Collapse multiple redundant columns into a single column
#'
#' Collapse multiple columns with redundant information into a single column. This function
#' is similar to dplyr::coalesce, but uses several checks and informative error messages.
#' Specifically, it checks for more than one unique non-NA value in each row, and returns an
#' error if there are any such rows (in similar cases, dplyr::coalesce uses the first
#' non-NA value). It then takes, for each row, the unique non-NA value found in one or
#' more of the input columns, and places that value into the first column.
#' @param x data frame containing the redundant columns to be coalesced/collapsed
#' @param columns character vector, the names of the columns to be coalesced/collapsed. The values will be combined into the first column listed in \code{columns}, and the remaining columns will be dropped.
#' @export
#' @return A data frame with \code{columns} values collapsed into the first column listed.
#' @usage coalesce_with_checks(x, columns)

coalesce_with_checks <- function(x, columns) {
  if (sum(columns %in% colnames(x)) < 2) # check for at least 2 columns in x
    stop(
      "Function coalesce_with_checks requires at least two valid columns in input data frame")
  if (any(apply(x[,columns], MARGIN=1, function(x) length(unique(na.omit(x)))) > 1))
    stop(
      paste(
        "Multiple non-NA values found in at least one row of annotation for the following columns:",
        paste(columns, collapse=", ")))
  
  columns_to_coalesce <- intersect(columns, colnames(x))
  if (!setequal(columns_to_coalesce, columns))
    warning("Some columns input to coalesce_with_checks were not found in input data object.")
  
  for (col.tmp in 2:length(columns)) {
    rows.tmp <- which(is.na(x[[columns[1]]]) & !is.na(x[[columns[col.tmp]]]))
    x[rows.tmp, columns[1]] <- x[rows.tmp, columns[col.tmp]]
    x[,columns[col.tmp]] <- NULL
  }
  
  return(x)
}
