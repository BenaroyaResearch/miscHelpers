#' Test if numbers are outside of a specified range
#'
#' Test if a vector of numbers is outside of a specified range, with the bounds optionally included.
#' @param x numeric vector to be tested.
#' @param rng vector of minimum and maximum values of the range. Elements beyond the first 2 are ignored.
#' @param inc_bounds logical, whether to include the bounds in the set that is tested. If set to TRUE, values identical to the bounds are considered matches. Defaults to FALSE. Works only if the function is not used as an infix operator (see usage example below).
#' @export
#' @return a logical vector with one element for each element in x
#' @usage \code{
#' x \%outside\% y
#' 
#' `\%outside\%`(x, y, inc_bounds=TRUE)}
"%outside%" <- function(x, rng, inc_bounds=FALSE) {
  if (inc_bounds) {x<=rng[1] | x>=rng[2]
  } else {x<rng[1] | x>rng[2]}
}
