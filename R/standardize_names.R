#' Standardize a character vector to standard syntax (deduplication copied from limma::makeUnique)
#'
#' Standardize all elements of a character vector to the same syntax. All letters are converted to lowercase, special characters are converted to underscores (snake_case) or removed (camelCase), and multiple and trailing (but not leading) special characters are removed. By default, elements are deduplicated by appending "_1" (snake_case) or "1" (camelCase) and so on.
#' @param x character vector with elements to be standardized
#' @param dedup logical, whether to deduplicate the vector elements. If TRUE, elements that are identical after standardization are deduplicated by appending "_1", "_2", etc. Defaults to TRUE.
#' @param case character, whether to standardize to "snake_case" or "camelCase" case. Defaults to "snake_case". Partial matches are okay.
#' @import magrittr
#' @import stringr
#' @import checkmate
#' @export
#' @return a character vector of the same length as \code{x}
#' @usage standardize_names(x, dedup = TRUE, case = "snake_case")
standardize_names <- function(x, dedup = TRUE, case = "snake_case") {
  # check input
  check_character(x)
  check_logical(dedup, len = 1)
  check_choice(case, c("snake_case", "camelCase"))
  
  if (case == "camelCase") {
    x <- x %>%
      str_trim() %>%
      str_to_lower() %>%
      make.names() %>%
      str_replace_all("[\\._]+","_") %>%
      str_replace("_$","") %>%
      gsub(pattern = "_([a-z])", replacement = "\\U\\1", x = ., perl = TRUE)
  } else if (case == "snake_case") {
    x <- x %>%
      str_trim() %>%
      str_to_lower() %>%
      make.names() %>%
      str_replace_all("[\\._]+","_") %>%
      str_replace("_$","")
  }
  if (dedup) {
    while (any(table(x) > 1)) {
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
                width = 1 + floor(log(n, 10)), flag = "0"), sep = ifelse(case == "snake_case", "_", ""))
        }
      }
    }
  }
  x
}

#' Standardize a character vector to standard syntax, defaulting to camelCase
#'
#' A synonym for \code{standardize_names}, but with \code{case} set to "camelCase" by default. Standardize all elements of a character vector to the same syntax. All letters are converted to lowercase, special characters are converted to underscores (snake_case) or removed (camelCase), and multiple and trailing (but not leading) special characters are removed. By default, elements are deduplicated by appending "_1" (snake_case) or "1" (camelCase) and so on.
#' @param x character vector with elements to be standardized
#' @param dedup logical, whether to deduplicate the vector elements. If TRUE, elements that are identical after standardization are deduplicated by appending "_1", "_2", etc. Defaults to TRUE.
#' @param case character, whether to standardize to "camelCase" or "snake_case" case. Defaults to "camelCase". Partial matches are okay.
#' @import checkmate
#' @export
#' @return a character vector of the same length as \code{x}
#' @usage standardizeNames(x, dedup = TRUE, case = "camelCase")
standardizeNames <- function(x, dedup = TRUE, case = "camelCase") {
  # check input
  check_character(x)
  check_logical(dedup, len = 1)
  check_choice(case, c("snake_case", "camelCase"))
  
  x <- standardize_names(x, dedup = dedup, case = case)
  
  x
}