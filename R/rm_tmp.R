#' Remove temporary objects from global environment
#'
#' Clear all temporary variables, after displaying them and (by default) prompting user. By default, the user is prompted with a list of the objects before they are removed. It is assuemd that temporary variables end in ".tmp" or have single-letter names.
#' @param ... optional arguments passed to \code{grep} via \code{rm_grep}.
#' @param ask logical, whether to prompt the user before removing objects. Defaults to TRUE.
#' @export
#' @usage rm_tmp(..., ask=TRUE)
rm_tmp <- function(..., ask=TRUE) rm_grep("^[a-zA-Z]$|\\.tmp", ask=ask, ...)
