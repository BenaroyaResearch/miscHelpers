#' Calculate the number of significant figures in a number (borrowed from ???)
#'
#' Calculate the number of significant figures in an integer or floating point number. NOTE: this function IS NOT vectorized. To run on a vector of numbers, use \code{sapply(x, sigDigs)}. Assumes that consecutive trailing 0s of integers and leading 0s of decimals are not significant. E.g. \code{sigDigs(10) == 1}.
#' @param n numeric, the number for which to determine significant figures.
#' @export
#' @return integer, the number of significant figures
#' @usage \code{sig_digs(n)}
sig_digs <- function(n) {
  i <- 0
  # Check for decimal point
  if(length(grep("\\.", toString(n))) > 0) { # real number
    # Separate integer and fractional parts
    intfrac <- unlist(strsplit(toString(n), "\\."))
    digstring <- paste(intfrac[1], intfrac[2], sep = "")
    numfigs <- nchar(digstring)
    while(i < numfigs) {
      # Find index of 1st non-zero digit from LEFT
      if(substr(digstring,i+1,i+1) == "0") {
        i <- i + 1
        next
      } else {
        sigfigs <- numfigs - i
        break
      }
    }   
  } else {  # must be an integer      
    digstring <- toString(n)
    numfigs <- nchar(digstring)
    while(i < numfigs) {
      # Find index of 1st non-zero digit from RIGHT
      if(substr(digstring, numfigs-i, numfigs-i) == "0") {
        i <- i + 1
        next
      } else {
        sigfigs <- numfigs - i
        break
      }
    }   
  }   
  sigfigs
}
