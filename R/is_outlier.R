#' Determine outliers
#' 
#' Determine outliers of a numeric vector, based on the first and third quartile minus/plus 1.5 times the
#' inter-quartile range.
#' @param x numeric vector containing values to test for outliers
#' @param na.rm logical, whether to remove NA values when calculating quantiles and IQR. Passed to \code{quantile()} and \code{IQR()}. Defaults to FALSE.
#' @export
#' @return logical vector, containing TRUE for outlier elements and FALSE for all other elements.
#' @usage is_outlier(x, na.rm=FALSE)
is_outlier <- function(x, na.rm=FALSE) {
  (x < quantile(x, 0.25, na.rm=na.rm) - 1.5 * IQR(x, na.rm=na.rm)) | (x > quantile(x, 0.75, na.rm=na.rm) + 1.5 * IQR(x, na.rm=na.rm))
}