#' List objects in global environment matching a regex pattern
#'
#' Determine the names of all objects in the global environment that match a specified regex pattern.
#' @param pattern character string, the regex pattern to match. Passed to \code{grep}.
#' @param value logical, whether to return the names of the objects. If set to FALSE, the position of the objects in the global environment object listing is returned. Defaults to TRUE.
#' @param envir the environment in which to search. Defaults to the global environment, \code{.GlobalEnv}.
#' @param ... optional arguments passed to \code{grep}.
#' @export
#' @return a vector of character names or numeric positions for the matches.
#' @usage ls_grep(pattern, value=TRUE, envir=.GlobalEnv, ...)
ls_grep <- function(pattern, value=TRUE, envir=.GlobalEnv, ...) {
  grep(pattern=pattern, x=base::ls(envir=envir), value=value, ...)
}
