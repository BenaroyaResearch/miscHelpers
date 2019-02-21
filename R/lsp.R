#' List package contents
#'
#' List the names of all objects in a package, optionally filtered for pattern matches. Borrowed from @_inundata
#' @param package the package object name (not quoted)
#' @param all.names logical, whether to include object names beginning with a '.'. Passed to \code{ls}.
#' @param pattern a regular expression, passed to \code{ls}. Only names matching \code{pattern} are returned.
#' @export
#' @return a vector of object names
#' @usage lsp(package, all.names=FALSE, pattern)
lsp <-function(package, all.names = FALSE, pattern) {
  package <- deparse(substitute(package))
  base::ls(
    pos = paste("package", package, sep = ":"),
    all.names = all.names,
    pattern = pattern
  )
}
