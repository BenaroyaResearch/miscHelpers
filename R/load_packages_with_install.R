#' Load a vector of packages, installing them if they are not already installed
#'
#' Load a vector of packages. First check if each one is installed, and install
#' it if not. This function works for CRAN, BioConductor, and github packages.
#' It can only operate on one type of package at a time. So if you are loading
#' packages from all three locations, you will need to call this function three
#' times.
#' @param packageNameList A character vector of package names. For github, the format should be "author/package".
#' @param location A character string specifying the location of the packages. Options are "CRAN", "BioConductor", and "github". Partial matches are allowed
#' @import checkmate
#' @importFrom utils install.packages
#' @importFrom remotes install_github
#' @export
#' @usage load_packages_with_install(packageNameList, location = "CRAN")
#' 
load_packages_with_install <- function(packageNameList, location = "CRAN") {
  # check inputs for validity
  checkmate::assert_character(packageNameList, min.len = 1)
  checkmate::assert_character(location, len = 1)
  location <- match.arg(location, c("CRAN", "BioConductor", "github"))
  
  # loop over each package in the list
  for (packageName in packageNameList) {
    # for github packages, extract versions of packageName with/without author
    if (location == "github") {
      packageNameWithAuthor <- packageName
      packageName <- unlist(strsplit(packageName, "/"))[2] 
    }
    
    # check if package is installed, and install if not
    if (!require(packageName, character.only = TRUE, quietly = TRUE)) {
      if (location == "CRAN") {
        install.packages(packageName)
      } else if (location == "BioConductor") {
        if (!require(BiocManager)) install.packages("BiocManager")
        BiocManager::install(packageName)
      } else if (location == "github") {
        if (!require(remotes)) install.packages("remotes")
        remotes::install_github(packageNameWithAuthor)
      }
    }
    
    # load the package
    suppressPackageStartupMessages(library(packageName, character.only = TRUE))
  }
}
