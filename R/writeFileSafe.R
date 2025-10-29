#' Safely Write an Output File
#'
#' This function wraps another function that produces a single output file.
#' It ensures that the file is only written if it does not already exist,
#' unless `overwrite = TRUE`. The function also allows you to specify which
#' argument of the wrapped function should receive the output filename (e.g.,
#' `file`, `path`, or `filename`).
#'
#' @param filename Character scalar; path to the output file.
#' @param fun A function that generates the output file (e.g., `write.csv`, `ggsave`, `saveRDS`).
#' @param file_arg Character string giving the name of the argument in `fun` that
#'   specifies the output file path (e.g., `"file"`, `"path"`, or `"filename"`).
#'   If `NULL`, the filename must be passed explicitly in `...`.
#' @param ... Additional arguments passed to `fun`.
#' @param overwrite Logical; if `TRUE`, allows overwriting an existing file. Defaults to `FALSE`.
#'
#' @return Invisibly returns the path to the output file.
#'
#' @examples
#' \dontrun{
#' # 1. write.csv uses the 'file' argument
#' writeFileSafe("iris.csv", write.csv, file_arg = "file", x = iris, row.names = FALSE)
#'
#' # 2. ggsave uses the 'filename' argument
#' library(ggplot2)
#' p <- ggplot(mtcars, aes(mpg, wt)) + geom_point()
#' writeFileSafe("plot.png", ggsave, file_arg = "filename", plot = p, width = 5, height = 4)
#'
#' # 3. saveRDS also uses 'file'
#' writeFileSafe("model.rds", saveRDS, file_arg = "file", object = lm(mpg ~ wt, data = mtcars))
#' }
#'
#' @export
writeFileSafe <- function(filename, fun, file_arg = NULL, ..., overwrite = FALSE) {
  # Validate inputs
  if (!is.character(filename) || length(filename) != 1) {
    stop("`filename` must be a single string.")
  }
  if (!is.function(fun)) {
    stop("`fun` must be a function.")
  }
  if (!is.null(file_arg) && !is.character(file_arg)) {
    stop("`file_arg` must be a character string or NULL.")
  }
  
  # Ensure directory exists
  dir <- dirname(filename)
  if (!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Skip or overwrite logic
  if (file.exists(filename) && !overwrite) {
    message("File '", filename, "' already exists. Skipping (set 'overwrite = TRUE' to replace).")
    return(invisible(filename))
  }
  
  # Prepare arguments list
  args <- list(...)
  if (!is.null(file_arg)) {
    # Insert or override the file path argument
    args[[file_arg]] <- filename
  }
  
  # Attempt to execute the function
  result <- try(do.call(fun, args), silent = TRUE)
  if (inherits(result, "try-error")) {
    stop("Error executing function: ", conditionMessage(attr(result, "condition")))
  }
  
  invisible(filename)
}
