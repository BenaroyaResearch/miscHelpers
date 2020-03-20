#' Convert dates in various formats to Date format
#'
#' This function is designed to convert dates from a mix of formats to standard "Date" format.
#' frames, data frame like objects (e.g. tibbles), or matrices. It returns an object of the same class.
#' The values that will be coerced to NAs can be provided as a character vector.
#' @param x vector, the date data to be converted
#' @param origin character, the base date for numeric date format, in the form "YYYY-MM-DD". Defaults to "1899-12-30".
#' @export
#' @return A vector of the same length as \code{x}
#' @usage standardize_dates(x)
standardize_dates <- function(x, origin="1899-12-30") {
  data_standardized <- as.Date(rep(NA, length(x)))

  # transform data from numeric form
  data_standardized[str_detect(x, "\\d{5}")] <-
    as.Date(
      as.numeric(
        x[str_detect(x, "\\d{5}")]),
      origin = "1899-12-30")

  # transform data from "YYYY-MM-DD" form
  data_standardized[
    str_detect(x, "\\d{4}\\-\\d{1,2}\\-\\d{1,2}")] <-
    as.Date(
      x[str_detect(x, "\\d{4}\\-\\d{1,2}\\-\\d{1,2}")],
      format = "%Y-%m-%d")

  # transform data from "M/DD/YYYY" form
  data_standardized[
    str_detect(x, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")] <-
    as.Date(
      x[str_detect(x, "\\d{1,2}\\/\\d{1,2}\\/\\d{4}")],
      format="%m/%d/%Y")

  data_standardized
}