#' Append text to object names in the global environment
#'
#' For a specified set of objects in the global environment, append text to the end of the object names. This is actually done by creating copies of the objects with new names and deleting the original versions, which may consume large amounts of memory if the objects are large. By default, the user is prompted before objects are renamed.
#' @param objects character vector, the names of the objects for which to append text. Can be a call to \code{ls_grep}, which allows pattern matching.
#' @param append_text character string, the text to append to each object's name.
#' @param sep character string, text to place as separator between object names and appended text. Defaults to "".
#' @param ask logical, whether to prompt the user before replacing objects. Defaults to TRUE.
#' @param envir the name of the environment in which to make the changes. Defaults to the global environment, \code{.GlobalEnv}.
#' @export
#' @usage append_objectnames(objects, append_text, sep="", ask=TRUE, envir=.GlobalEnv)
append_objectnames <- 
  function(objects, append_text, sep="", ask=TRUE, envir=.GlobalEnv) {
    
    objects.drop <- objects[!(objects %in% base::ls(envir=envir))]
    warning("The following objects were not found in the specified environment:\n", paste(objects.drop, sep="", collapse=", "))
    objects <- objects[objects %in% base::ls(envir=envir)]
    
    if (ask) {
      cat("About to replace the following objects:\n\n")
      cat(objects, sep = "\n")
      proceed <- read_yn("Do you want to proceed? (Enter y or n): ")
      if (proceed == "n") 
        stop("Objects not replaced.")
    }
    for (i in objects) {
      assign(paste(i, append_text, sep=sep), get(i, envir=envir), envir=envir)
      rm(list=i, envir=envir)
    }
  }
