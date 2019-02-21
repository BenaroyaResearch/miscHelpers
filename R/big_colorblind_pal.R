#' Generate a large colorblind palette
#'
#' Generate a large colorblind palette, optionally with the colors shuffled to make adjacent colors more distinctive. This is a wrapper for colorblind_pal() and colorRampPalette(). It also drops the difficult-to-see yellow of colorblind_pal
#' @param n_colors integer, the number of colors in the palette. No default. Anything less than the number of colors in colorblind_pal (after dropping yellow and/or black) just returns the colorblind_pal colors.
#' @param shuffle_colors logical, whether to shuffle the palette order. Defaults to FALSE.
#' @param drop_yellow logical, whether to drop the difficult-to-see yellow color. Defaults to TRUE.
#' @param drop_black logical, whether to drop the black from the palette. Defaults to FALSE.
#' @export
#' @return a character vector with the colors
#' @usage
#' big_colorblind_pal(
#'   n_colors,
#'   shuffle_colors=FALSE,
#'   drop_yellow=TRUE, drop_black=FALSE)
big_colorblind_pal <-
  function(n_colors, shuffle_colors=FALSE, drop_yellow=TRUE, drop_black=FALSE) {
    pal.start <- ggthemes::colorblind_pal()(8)
    if (drop_yellow) pal.start <- pal.start[-5]
    if (drop_black) pal.start <- pal.start[-1]
    if (n_colors <= length(pal.start)) {
      pal.colorblind <- pal.start[1:n_colors]
    } else {
      pal.colorblind <- colorRampPalette(pal.start)(n_colors)
    }
    
    if (shuffle_colors) pal.colorblind <- sample(pal.colorblind)
    
    pal.colorblind
}
