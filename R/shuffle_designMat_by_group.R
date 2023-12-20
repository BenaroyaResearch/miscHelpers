#' Shuffle a variable in a design matrix within studies or groups
#' 
#' This function is designed to shuffle annotation data among samples within studies or groups.
#' It is designed to test the effect of assocations between variables across studies or groups,
#' by allowing comparison of the trend recovered if all relationships within the data are
#' retained EXCEPT FOR the values of the variable of interest.
#' @param designMat a design matrix.
#' @param shuffle_col the name or number of the column of \code{designMat} to shuffle.
#' @param group_cols the names or numbers of the columns of \code{designMat} corresponding to the groups/studies. Values of the shuffled column are resampled WITHIN group/study
#' @export
#' @return a design matrix, with dimentions identical to the input \code{designMat}
#' @usage shuffle_designMat_by_group(designMat, shuffle_col, group_cols)
shuffle_designMat_by_group <- 
  function(designMat, shuffle_col, group_cols) {
    designMat[rowSums(designMat[, group_cols]) == 0, shuffle_col] <-
      sample(designMat[rowSums(designMat[, group_cols]) == 0, shuffle_col])
    for (i in group_cols) {
      designMat[designMat[, i] == 1, shuffle_col] <- 
        sample(designMat[designMat[,i] == 1, shuffle_col])
    }
    return(designMat)
}