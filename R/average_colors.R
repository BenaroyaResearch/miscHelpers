#' Determine the numerical average of a vector of colors
#'
#' Convert a vector of colors into a single color that averages their numerical values of RGB (red-green-blue) or Lab (lightness-a-b) characteristics.
#' @param x vector of color names to average
#' @param method character, specificying the color space in which to average. Currently only "RGB" is accepted.
#' @importFrom grDevices rgb col2rgb 
#' @export
#' @return character, the hexadecimal representation of the color
#' @usage average_colors(x, method="RGB")
#' @details Solution derived from answer by stackoverflow user Deleplace at https://stackoverflow.com/questions/14482226/how-can-i-get-the-color-halfway-between-two-colors
average_colors <- function(x, method = "RGB") {
  method <- match.arg(method, choices = c("RGB"))
  x <- col2rgb(x, alpha = TRUE)
  x <- apply(x, 1, mean)
  if (x["alpha"] == 255) {
    x <- rgb(x[1], x[2], x[3], maxColorValue = 255)  # remove alpha if they're all 255
  } else x <- rgb(x[1], x[2], x[3], x[4], maxColorValue = 255)
  x
}
