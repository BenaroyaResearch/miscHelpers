#' Find and replace text in object names in the global environment
#'
#' Search all object names in the global environment for a regex pattern, and replace that pattern with specified text. This is actually done by creating copies of the objects with new names and deleting the original versions, which may consume large amounts of memory if the objects are large. By default, the user is prompted before objects are renamed.
#' @param pattern character string, the regex pattern to replace. Passed to \code{grep} via \code{ls_grep}.
#' @param replace character string, the text with which to replace \code{pattern}.
#' @param pattern_find optional character string used to determine objects. Can be different from \code{pattern}, which allows renaming a subset of objects in the global environment. Defaults to NULL, which results in objects being matched with \code{pattern}.
#' @param ask logical, whether to prompt the user before replacing objects. Defaults to TRUE.
#' @param envir the name of the environment in which to make the changes. Defaults to the global environment, \code{.GlobalEnv}.
#' @importFrom stringr str_replace
#' @export
#' @usage find_replace(pattern, replace, pattern_find=NULL, ask=TRUE, envir=.GlobalEnv)
find_replace <- function(pattern, replace, pattern_find=NULL, ask=TRUE, envir=.GlobalEnv) {
  if (is.null(pattern_find)) pattern_find <- pattern
  vars_to_replace <- ls_grep(pattern_find)
  if (ask) {
    cat("About to replace the following objects:\n\n")
    cat(vars_to_replace, sep = "\n")
    proceed <- read_yn("Do you want to proceed? (Enter y or n): ")
    if (proceed == "n") 
      stop("Variables not replaced.")
  }
  for (i in vars_to_replace) {
    assign(str_replace(i, pattern, replace), get(i, envir=envir), envir=envir)
    rm(list=i, envir=envir)
  }
}

