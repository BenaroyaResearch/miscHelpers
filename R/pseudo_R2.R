#' Calculate a pseudo R^2 value from a fitted model
#'
#' Calculate the squared correlation of the fitted and observed values for the dependent variable, as a pseudo-R^2. Inputs can be models fit by lmer or lm (other formats to be incorporated later).
#' @param x result from model fitting, generally the output of lm or lmer.
#' @export
#' @return numeric, the squared correlation of the fitted values and the observed values of the dependent variable
#' @usage \code{pseudo_R2(x)}
pseudo_R2 <- function(x) {
  if (class(x) %in% c("lm")) {
    R2 <-
      cor(fitted(x), x$model[,1]) ^ 2
  } else if (class(x) %in% c("merModLmerTest", "lmerMod", "merMod")) {
    R2 <-
      cor(fitted(x), x@frame[,1]) ^ 2
  } else stop(paste("Not sure what to do with object of class", class(x)))
  R2
}
