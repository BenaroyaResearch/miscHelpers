#' Remove temporary objects from global environment
#'
#' Clear all temporary variables, after displaying them and (by default) prompting user. By default, the user is prompted with a list of the objects before they are removed. It is assuemd that temporary variables end in ".tmp" or have single-letter names.
#' @param ... optional arguments passed to \code{grep} via \code{rm_grep}.
#' @export
#' @usage \code{rm_tmp(...)}
rm_tmp <- function(...) rm_grep("^[a-zA-Z]$|\\.tmp", ...)
