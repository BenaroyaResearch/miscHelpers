#' Add transparency to a vector of colors
#'
#' Add a transparency factor to each color in a vector of color names, using alpha on a scale of 0-255. Adapted from http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color.
#' @param color_names vector of color names to be modified.
#' @param alpha numeric, the alpha value, between 0 (completely transparent) and 255 (the original color). Passed to \code{rgb}.
#' @export
#' @return a character vector of the same length as \code{color_names}
#' @usage \code{makeTransparent(color_names, alpha=100)}
makeTransparent<-function(color_names, alpha=100) {
  new_colors<-col2rgb(color_names)
  apply(new_colors, 2,
        function(x){rgb(red=x[1], green=x[2], blue=x[3],
                        alpha=alpha, maxColorValue=255)})
}
