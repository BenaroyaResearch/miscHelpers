#' Convert dates in various formats to Date format
#'
#' This function is designed to convert dates from a mix of formats to standard "Date" format. It can handle dates in POSIX, numeric, Date, or mixed character format.
#' @param x vector, the date data to be converted
#' @param origin character, the base date for numeric date format, in the form "YYYY-MM-DD". Defaults to "1899-12-30".
#' @export
#' @return A vector of the same length as \code{x}
#' @usage standardize_dates(x)
standardize_dates <- function(x, origin="1899-12-30") {

  if (inherits(x, "Date")) {

    # keep simple Date format
    data_standardized <- x

  } else if (inherits(x, "integer") | inherits(x, "numeric")) {

    data_standardized <- as.Date(rep(NA, length(x)))

    # transform from numeric/integer date type
    regex.tmp <- "^\\d{5}$"
    data_standardized[
      !is.na(x) & str_detect(as.character(x), regex.tmp)] <-
      as.Date(
        x[!is.na(x) & str_detect(as.character(x), regex.tmp)], origin = origin)

    # transform from numeric YYYYMMDD format
    regex.tmp <- "^(19|20)\\d{2}[0-1]\\d[0-3]\\d$"
    date_format.tmp <- "%Y%m%d"
    data_standardized[
      !is.na(x) & str_detect(as.character(x), regex.tmp)] <-
      as.Date(
        as.character(x[!is.na(x) & str_detect(as.character(x), regex.tmp)]),
        format = date_format.tmp)

  } else if (inherits(x, "POSIXt")) {

    # transform from POSIX
    data_standardized <- as.Date(x)

  } else if (inherits(x, "character")) {

    # transform from character

    data_standardized <- as.Date(rep(NA, length(x)))

    # transform data from numeric form in character vector
    regex.tmp <- "^\\d{5}$"
    data_standardized[
      !is.na(x) &
        str_detect(x, regex.tmp)] <-
      as.Date(
        as.numeric(
          x[
            !is.na(x) &
              str_detect(x, regex.tmp)]),
        origin = origin)

    # transform data from "YYYY-MM-DD" form
    regex.tmp <- "^\\d{4}\\-\\d{1,2}\\-\\d{1,2}$"
    date_format.tmp <- "%Y-%m-%d"
    data_standardized[
      !is.na(x) & str_detect(x, regex.tmp)] <-
      as.Date(
        x[!is.na(x) & str_detect(x, regex.tmp)],
        format = date_format.tmp)

    # transform data from "M/DD/YYYY" form
    regex.tmp <- "^\\d{1,2}\\/\\d{1,2}\\/\\d{4}$"
    date_format.tmp <- "%m/%d/%Y"
    data_standardized[
      !is.na(x) & str_detect(x, regex.tmp)] <-
      as.Date(
        x[!is.na(x) & str_detect(x, regex.tmp)],
        format = date_format.tmp)

    # transform data from "M/D/YY" form
    regex.tmp <- "^\\d{1,2}\\/\\d{1,2}\\/\\d{2}$"
    date_format.tmp <- "%m/%d/%y"
    data_standardized[
      !is.na(x) & str_detect(x, regex.tmp)] <-
      as.Date(
        x[!is.na(x) & str_detect(x, regex.tmp)],
        format = date_format.tmp)

    # transform data from "YYYYMMDD" form
    regex.tmp <- "^\\d{8}$"
    date_format.tmp <- "%Y%m%d"
    data_standardized[
      !is.na(x) & str_detect(x, regex.tmp)] <-
      as.Date(
        x[!is.na(x) & str_detect(x, regex.tmp)],
        format = date_format.tmp)
  } else stop("No recognized date format found in input data")

  data_standardized
}