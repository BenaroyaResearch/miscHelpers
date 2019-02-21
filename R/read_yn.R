#' Prompt user for a yes or no answer

#' Prompt the user for a yes or no answer, and repeat until an acceptable response is entered
#' @param display_text character string, the text to be displayed at the prompt.
#' @importFrom stringr str_to_lower
#' @export
#' @return character, either 'y' or 'n'. Uppercase responses are converted to lowercase.
#' @usage read_yn(display_text)
read_yn <- function(display_text) {
  answer <- str_to_lower(readline(prompt=display_text))
  if (answer %nin% c('y', 'n')) answer <- read_yn(display_text)
  return(answer)
}
