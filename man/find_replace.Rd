% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/find_replace.R
\name{find_replace}
\alias{find_replace}
\title{Find and replace text in object names in the global environment}
\usage{
find_replace(
  pattern, replace, pattern_find = NULL, ask = TRUE, envir = .GlobalEnv)
}
\arguments{
\item{pattern}{character string, the regex pattern to replace. Passed to \code{grep} via \code{ls_grep}.}

\item{replace}{character string, the text with which to replace \code{pattern}.}

\item{pattern_find}{optional character string used to determine objects. Can be different from \code{pattern}, which allows renaming a subset of objects in the global environment. Defaults to NULL, which results in objects being matched with \code{pattern}.}

\item{ask}{logical, whether to prompt the user before replacing objects. Defaults to TRUE.}

\item{envir}{the name of the environment in which to make the changes. Defaults to the global environment, \code{.GlobalEnv}.}
}
\description{
Search all object names in the global environment for a regex pattern, and replace that pattern with specified text. This is actually done by creating copies of the objects with new names and deleting the original versions, which may consume large amounts of memory if the objects are large. By default, the user is prompted before objects are renamed.
}
