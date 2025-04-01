#' Calculate specificity scores for a set of targets and controls, based on the beta distribution
#'
#' Calculate specificity scores for a set of targets and controls. Most often used for single-cell feature barcode data, but may be useful for other applications where a positive signal needs to be distinguished from a variable background. Adapted from T-BEAM, 10x Genomics's approach to monomeric MHC antibody specificity scoring. The return is a list structure, with each list element containing a dataframe of specificity scores for each target. Likelihood scores are determined via a cumulative beta distribution function with a confidence of .925. 
#' @param counts numeric matrix, a matrix of counts with markers names as the rownames and the sample identifiers (single cells, libraries, ect) as the column names. Matrix needs to contain a row for each target and control, but can contain non-relevant rows not being used in this calculation.
#' @param targets character vector, names of the target markers for which to test specificity. All values must appear in row names of \code{counts}.
#' @param controls character vector, names of the control markers to be used in build null distributions for specificity. All values must appear in row names of \code{counts}.
#' @param nPrior integer, pseudoCount for the control count to be added to the count from \code{counts}. Higher numbers for nPrior may be appropriate if you have reason to suspect noisy data and want to make high likelihood scores more difficult to achieve by chance. Defaults to 3.
#' @param sPrior integer, pseudoCount for the target count to be added to the count from \code{counts}. Higher ratios of this to nPrior will results in lower discrimination between false and true positives, but better elimination of false negatives. Defaults to 1. 
#' @export
#' @return a data frame of specificity scores, with one row per sample and one column for each target marker. Each value is a likelihood score for specificity for that list's antigen
#' @importFrom stats pbeta
#' @usage
#' scoreSpecificity(
#'   counts, targets, controls,
#'   nPrior = 3, sPrior = 1)

scoreSpecificity <- function(counts, targets, controls, nPrior = 3, sPrior = 1) {
  # check inputs for validity
  if (!(inherits(counts, "matrix") | inherits(counts, "Matrix")))
    stop("Input `counts` object must be a matrix.")
  checkmate::assertNumeric(as.matrix(counts))
  checkmate::assertCharacter(targets)
  checkmate::assertCharacter(controls)
  checkmate::assertIntegerish(nPrior)
  checkmate::assertIntegerish(sPrior)
  
  if (!all(targets %in% rownames(counts))) {
    stop("All targets must be present in the rownames of the counts matrix")
  }
  
  if (!all(controls %in% rownames(counts))) {
    stop("All controls must be present in the rownames of the counts matrix")
  }
  
  # initialize dataframe for storing output
  scores <- data.frame(samplename = colnames(counts))
  
  # loop over targets argument
  for (target in targets) {
    # initialize vector for storing value from each control
    scoresCurrTarget <- c()
    for (controlNum in seq_len(length(controls))) {
      # get 1 - pvalue for each control for the target of the current iteration
      scoresCurrTarget <- 
        cbind(
          scoresCurrTarget, 
          (1 - (pbeta(.925, 
                      as.numeric(counts[target,]) + sPrior, 
                      as.numeric(counts[controls[controlNum],]) + nPrior))) * 100)
    }
    scores[, target] <- rowMeans(scoresCurrTarget)
  }
  
  return(scores)
}