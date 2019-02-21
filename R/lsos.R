#' List the N largest objects in the global environment
#'
#' Lists the largest objects in the global environment, by memory usage.
#' @param ... optional arguments, passed to \code{ls.objects}
#' @param n integer, the number of objects to list. Defaults to 10.
#' @export
#' @usage lsos(..., n=10)
lsos <- function(..., n=10) {
  ls.objects(..., order.by="Size", decreasing=TRUE, head=TRUE, n=n)
}
