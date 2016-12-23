#' Remove objects matching a regex pattern from global environment
#'
#' Identify and remove all objects in the global environment that match a specified regex pattern. By default, the user is prompted with a list of the objects before they are removed.
#' @param rm_pattern character string, the regex pattern to match. Passed to \code{grep}.
#' @param ask logical, whether to prompt the user before removing objects. Defaults to TRUE.
#' @param ... optional arguments passed to \code{grep}.
#' @export
#' @usage \code{rm_grep(rm_pattern, ask=TRUE, ...)}
rm_grep <- function(rm_pattern, ask=TRUE, ...) {
  vars_to_remove <- grep(rm_pattern, base::ls(envir=.GlobalEnv), value=TRUE, ...)
  if (ask) {
    cat("About to remove the following objects:\n\n")
    cat(vars_to_remove, sep="\n")
    proceed <- read_yn("Do you want to proceed? (Enter y or n): ")
    if (proceed == "n") stop("Variables not removed.")
  }
  rm(list=vars_to_remove, envir=.GlobalEnv)
}
