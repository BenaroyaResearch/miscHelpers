#' Insert line breaks at spaces with hyphen-aware wrapping and a hard cutoff
#'
#' Wrap text by inserting newline characters at the first space after \code{maxWidth}, optionally splitting long words at hyphens/dashes, and enforcing a hard maximum word length.
#'
#' @param textVector character vector of strings to break
#' @param maxWidth integer (> 0), target wrap width in characters; wrapping occurs at the first space after this width. Default is \code{40}.
#' @param hardFactor numeric (> 1), a multiplier applied to \code{maxWidth} to compute the hard maximum length for a single word. Words longer than \code{ceiling(maxWidth * hardFactor)} are cut at that point and hyphenated. Default is \code{1.5}.
#' @param linebreak character, the string to use for line breaks; defaults to "\n"
#'
#' @details
#' \itemize{
#'   \item \strong{Normal wrapping:} lines break at spaces; words are not split.
#'   \item \strong{Hyphen/dash rule:} if a single word is longer than \code{maxWidth}
#'     but contains a hyphen or dash (\code{"-"}, en dash \code{"–"}, or em dash \code{"—"}),
#'     the word may be split at those characters, preserving the hyphen/dash at the line end.
#'   \item \strong{Hard cutoff:} if a word exceeds \code{ceiling(maxWidth * hardFactor)},
#'     it is cut at that limit and a hyphen (\code{"-"}) is appended.
#'   \item Words longer than \code{maxWidth} that do \emph{not} contain a hyphen/dash
#'     are allowed to extend beyond \code{maxWidth} unless they exceed the hard cutoff.
#'   \item The function is vectorized over \code{textVector}; the output length matches the input.
#' }
#'
#' @return A character vector where each element contains line breaks at the computed wrap points.
#'
#' @examples
#' # Basic wrapping at spaces
#' insertLineBreaks("This wraps at spaces after the target width.", maxWidth = 20)
#'
#' # Splitting on hyphens/dashes when a single word exceeds maxWidth
#' insertLineBreaks("A long-term-relationship example that should wrap nicely.", maxWidth = 20)
#'
#' # Hard cutoff for a very long unbroken word (will be hyphenated at the cutoff)
#' insertLineBreaks("ThisSuperRidiculouslyLongUnbrokenWordWithoutHyphens",
#'                  maxWidth = 20, hardFactor = 1.5)
#'
#' # Vectorized input
#' insertLineBreaks(c("First line to wrap", "Second line that is a bit longer"), maxWidth = 18)
#'
#' @export

insertLineBreaks <-
  function(textVector, maxWidth = 40, hardFactor = 1.5, linebreak = "\n") {
    hardMax <- ceiling(maxWidth * hardFactor)
    
    processText <- function(text) {
      words <- strsplit(text, " ")[[1]]
      lines <- c()
      currentLine <- ""
      
      for (word in words) {
        # Case 1: Word exceeds hard maximum — force split
        if (nchar(word) > hardMax) {
          splitPoint <- hardMax - 1
          word <- paste0(substr(word, 1, splitPoint), "-")
        }
        
        # Case 2: Word longer than maxWidth but contains a hyphen or dash
        else if (nchar(word) > maxWidth && grepl("[-–—]", word)) {
          parts <- unlist(strsplit(word, "[-–—]"))
          # Reinsert hyphens between parts, preserving them
          parts <- paste0(parts, c(rep("-", length(parts) - 1), ""))
          for (part in parts) {
            if (nchar(currentLine) + nchar(part) + 1 > maxWidth) {
              lines <- c(lines, trimws(currentLine))
              currentLine <- part
            } else {
              currentLine <- paste(currentLine, part)
            }
          }
          next
        }
        
        # Normal wrapping
        if (nchar(currentLine) > 0 && nchar(currentLine) + nchar(word) + 1 > maxWidth) {
          lines <- c(lines, trimws(currentLine))
          currentLine <- word
        } else {
          currentLine <- paste(currentLine, word)
        }
      }
      
      lines <- c(lines, trimws(currentLine))
      paste(lines, collapse = linebreak)
    }
    
    sapply(textVector, processText, USE.NAMES = FALSE)
  }
