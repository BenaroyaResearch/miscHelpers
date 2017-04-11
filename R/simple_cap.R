#' Capitalize the first letter of each element and/or word in a vector of character strings
#'
#' Capitalize the first letter of each word in each element of a character vector. By default, words are assumed to be separated " ".
#' @param x character vector, with elements to be capitalized.
#' @param sep_str character string, the separator between words within vector elements. Vector elements are split on this string, each chunk is capitalized, and they are combined with this string as separator. Defaults to " ".
#' @export
#' @return a character vector of the same length as \code{x}.
#' @usage \code{simple_cap(x, sep_str=" ")}
simple_cap <- function(x, sep_str=" ") {
  s <- strsplit(x, sep_str)
  sapply(
    s,
    function(y) {paste(toupper(substring(y,1,1)), substring(y,2),
                       sep="", collapse=sep_str)})
}
