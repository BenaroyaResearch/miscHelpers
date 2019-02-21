#' Labeller function for plotting WGCNA modules in facet_grid or facet_wrap
#'
#' This is a function to pass facet labels to ggplot functions \code{facet_grid} and \code{facet_wrap},
#' so that the facets have the module color name as the label. Basically, it strips off the "ME" and
#' returns the label in a format that ggplot understands.
#' @param variable not used by this function. Included for compatibility with \code{facet_grid} and \code{facet_wrap}.
#' @param value character vector containing the levels of the factor used for facetting. Generally passed directly to the function by \code{facet_grid} or \code{facet_wrap}.
#' @importFrom stringr str_replace
#' @export
#' @return An object of the same dimension as \code{value}, with any "ME" strings removed from each element.
#' @usage ggplot2::facet_grid(.~variable, labeller=me_labeller)
me_labeller <- function(variable, value) {
  return(str_replace(value, "ME", ""))
}
