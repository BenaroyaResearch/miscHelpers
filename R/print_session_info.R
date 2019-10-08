#' Print session info with better formatting
#'
#' Print the session information. This function is a wrapper for \code{sessioninfo::session_info()}, with
#' the list of packages formatted using \code{kableExtra::kable_styling()} to make it more adaptive to
#' the output context. In particular, this function works much better with knitr output to html or pdf.
#' In its current state, there are no parameters, and only default settings. Additional flexibility, 
#' such as controlling the fields that are output, may be added in the future.
#' @export
#' print_session_info()
print_session_info <- function() {
  ses_info <- sessioninfo::session_info()
  print(ses_info$platform)
  kableExtra::kable_styling(
    kable(
      dplyr::select(ses_info$packages, package, version=loadedversion, date, source),
      row.names=FALSE))
}