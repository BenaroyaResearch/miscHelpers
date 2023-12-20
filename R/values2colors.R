#' Convert a vector of numeric values to a vector of colors
#'
#' Convert a vector of numeric values to a vector of color names. The input vector is divided into a number
#' of intervals, set by \code{breaks}. Values in each interval are assigned a color along a scale generated
#' by \code{colorRampPalette}.
#' @param x numeric vector containing values to be converted to colors
#' @param col.start,col.end color names indicating the start and end of the color ramp. Passed to \code{colorRampPalette}.
#' @param col.pal name of a palette from \code{RColorBrewer}; an alternative way to specify color palette. The specified palette will be passed to \code{colorRampPalette}.
#' @param na.col color name to use for NA values in \code{x}. Defaults to "grey50".
#' @param breaks number of intervals to break the values of \code{x} into. Defaults to 10.
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
#' @return a vector of color names, of length equivalent to the length of x.
#' @usage
#' values2colors(x,
#'   col.start = "blue", col.end = "red",
#'   col.pal = NULL, na.col = "grey50",
#'   breaks = 10)
values2colors <-
  function(
    x,
    col.start = "blue", col.end = "red",
    col.pal = NULL, na.col = "grey50",
    breaks = 10) {
    if (!is.numeric(x)) stop("Input vector is not numeric.")
    
    if (is.null(col.pal)) {
      cols <- colorRampPalette(colors = c(col.start, col.end))(breaks)
    } else {
      n <- brewer.pal.info[col.pal, "maxcolors"]
      cols <- colorRampPalette(brewer.pal(n, col.pal))(breaks)
    } 
    
    cols.out <- cols[as.numeric(cut(x, breaks = breaks))]
    cols.out[is.na(cols.out)] <- na.col
    
    return(cols.out)
  }
