#' Determine the redundant columns in a model matrix
#'
#' This function identifies the redundant columns in a model matrix, if the model matrix is not of full rank.
#' @param model_matrix the model matrix, typically the result of a call to \code{model.matrix} or similar functions.
#' @importFrom ordinal drop.coef
#' @export
#' @return a (possibly empty) character vector, with the names of the redundnat columns. Note that these names will typically include the variable name and (if the variable is treated as a factor) the name of the redundant level(s).
#' @usage redundant_cols.model.matrix(model_matrix)
redundant_cols.model.matrix <- function(model_matrix) {
  setdiff(colnames(model_matrix),
          colnames(ordinal::drop.coef(model_matrix)))
}