#' Improved listing of objects (from Dirk Eddelbuettel)
#'
#' This is an alternative to the base \code{ls} function, with additional functionality. Documentation to be written.
#' @param pos numeric, position in search path
#' @param pattern character, pattern argument passed to \code{ls}
#' @param order.by character, order by this column (optional)
#' @param decreasing logical, order in decreasing order by \code{order.by} (optional, and ignored if \code{order.by} is not specified)
#' @param head logical, only show the first few rows (optional)
#' @param n numeric, number of rows to show (optional, and ignored if \code{head} is not specified)
#' @importFrom utils object.size
#' @export
#' @usage
#' ls.objects(
#'   pos = 1, pattern, order.by, decreasing = FALSE, head = FALSE, n = 5)
ls.objects <- function(pos = 1, pattern, order.by,
                        decreasing = FALSE, head = FALSE, n = 5) {
  napply <- function(names, fn) sapply(names, function(x)
    fn(get(x, pos = pos)))
  names <- base::ls(pos = pos, pattern = pattern)
  obj.class <- napply(names, function(x) as.character(class(x))[1])
  obj.mode <- napply(names, mode)
  obj.type <- ifelse(is.na(obj.class), obj.mode, obj.class)
  obj.size <- napply(names, object.size)
  obj.dim <- t(napply(names, function(x)
    as.numeric(dim(x))[1:2]))
  vec <- is.na(obj.dim)[, 1] & (obj.type != "function")
  obj.dim[vec, 1] <- napply(names, length)[vec]
  out <- data.frame(obj.type, obj.size, obj.dim)
  names(out) <- c("Type", "Size", "Rows", "Columns")
  if (!missing(order.by))
    out <- out[order(out[[order.by]], decreasing = decreasing), ]
  if (head)
    out <- head(out, n)
  out
}
