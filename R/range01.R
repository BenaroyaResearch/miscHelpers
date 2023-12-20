#' Normalize a numeric vector to 0-1 range
#' 
#' This function normalizes a numeric vector to have minimum of 0 and maximum of 1, with values in between linearly
#' transformed. It ignores NA values in the calculation of min and max.
#' @param x numeric vector to be normalized
#' @export
#' @usage range01(x)
range01 <- 
  function(x) ((x - min(x, na.rm = TRUE)) / 
                 (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
