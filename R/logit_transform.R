#' Transform a vector of proportions using the logit transformation
#'
#' Transform a vector of values between 0 and 1 using the logit transformation.
#' Includes a buffer, which by default nudges 0s and 1s inward to avoid infinite values.
#' @param x vector of values to transform. Must be between 0 and 1, inclusive.
#' @param buffer numeric, the amount to nudge limit values inwards before transformation to avoid returning infinite values. Defaults to 0.001. Set to 0 to return infinite values. Setting to "estimate" will use the smallest distance from 0 or 1 in \code{x} as the buffer.
#' @export
#' @return a vector of the transformed values
#' @usage logit_transform(x, buffer = 0.001)
logit_transform <- function(x, buffer = 0.001) {
  if (any(x > 1 | x < 0, na.rm = TRUE)) stop("Input vector contains values outside the range [0,1].")
  if (buffer == "estimate")
    buffer <- min(abs(c(x, 1 - x))[abs(c(x, 1 - x)) != 0], na.rm = TRUE)
  x[x %in% 0] <- x[x %in% 0] + buffer
  x[x %in% 1] <- x[x %in% 1] - buffer
  log(x/(1-x))
}
