#' Re-order a data frame for plotting points in ggplot
#'
#' Re-order a data frame to control the plotting order of points in ggplot. This is designed to
#' control over-plotting of points by sorting the data frame based on input arguments. It is
#' used in plots by symbol and color, where overplotting may affect readability.
#' @param x data frame containing data to re-order
#' @param method character string, specifying how to order the rows. Currently accepted values are "random", which randomizes the order, and "input", which returns the input data frame as-is.
#' @export
#' @return a data frame with same contents as \code{x}, with rows re-ordered according to \code{method}.
#' @usage \code{
#' order_points(x, method)}
order_points <- function(x, method) {
  method <- match.arg(method, choices=c("input", "random"))
  switch(method,
         "input" = x,
         "random" = x[sample.int(nrow(x)),])
}
