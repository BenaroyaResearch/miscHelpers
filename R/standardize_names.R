#' Standardize a character vector to standard syntax (deduplication copied from limma::makeUnique)
#'
#' Standardize all elements of a character vector to the same syntax. All letters are converted to lowercase, special characters are converted to underscores, and multiple and trailing (but not leading) special characters are removed. By default, elements are deduplicated by appending "_1" and so on.
#' @param x character vector with elements to be standardized
#' @param dedup logical, whether to deduplicate the vector elements. If TRUE, elements that are identical after standardization are deduplicated by appending "_1", "_2", etc. Defaults to TRUE.
#' @import stringr
#' @export
#' @return a character vector of the same length as \code{x}
#' @usage \code{standardize_names(x, dedup=TRUE)}
standardize_names <- function(x, dedup=TRUE) {
  x <- x %>%
    str_trim() %>%
    str_to_lower() %>%
    make.names() %>%
    str_replace_all("[\\._]+","_") %>%
    str_replace("_$","")
  if (dedup) {
    tab <- table(x)
    tab <- tab[tab > 1]
    lentab <- length(tab)
    if (lentab > 0) {
      u <- names(tab)
      for (i in 1:lentab) {
        n <- tab[i]
        x[x == u[i]] <-
          paste(
            x[x == u[i]],
            formatC(
              1:n, 
              width = 1 + floor(log(n, 10)), flag = "0"), sep = "_")
      }
    }
  }
  x
}
