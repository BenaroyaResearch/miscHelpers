#' Determine non-matches of one vector to another
#'
#' Determine the members of one vector that are NOT found in another vector.  This is equivalent to
#' \code{!(x \%in\% y)}, but is a useful shortcut. Borrowed from Stephen Turner.
#' @param x vector to be matched to elements in \code{y}.
#' @param y vector to be searched for elements of \code{x}.
#' @export
#' @return a logical vector with one element for each element in x
#' @usage x \%nin\% y
"%nin%" <- function(x, y) {
  !(x %in% y)
}
