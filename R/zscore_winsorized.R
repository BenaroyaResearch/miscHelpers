#' Normalize a numeric vector to z-scores with the values winsorized (capped at a maximum absolute value)
#' 
#' This function normalizes a numeric vector to z-scores (mean of 0 and standard
#' deviation of 1), with values beyond a specified threshold winsorized (capped
#' at a maximum absolute value). It ignores NA values in the calculation of
#' mean and standard deviation, but returns NAs in the output vector.
#' @param x numeric vector to be normalized
#' @param threshold the maximum absolute value to which the vector will be winsorized
#' @export
#' @usage range01(x, threshold = 3)
zscore_winsorized <- function(x, threshold = 3) {
  x <- scale(x)[,1]
  x[x > threshold] <- threshold
  x[x < -threshold] <- -threshold
  return(x)
}
