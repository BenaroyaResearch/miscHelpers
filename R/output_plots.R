#' Output plots with specified format and checking for existing file
#'
#' This function outputs plots with a specified format, while allowing checks for whether to overwrite
#' an existing file. It is basically a wrapper on \code{pdf}, \code{png}, and \code{plotscale::as.pdf}.
#' @name output_plots
#' @param x plot object
#' @param fileDir directory to save the file. Defaults to "plots", which outputs to a subfolder in the working directory. If the specified directory does not exist, the function will create it.
#' @param fileStem the stem of the file name, without the extension
#' @param plotFormat character vector of plot formats to output. Can be any combination of "plotscalePdf", "pdf", and "png".
#' @param width width of the plot or graphics output in inches; for plotscalePdf, this is the width of the plot area, not the graphics output area
#' @param height height of the plot or graphics output; for plotscalePdf, this is the width of the plot area, not the graphics output area
#' @param pngRes resolution of the PNG file in dpi. Ignored if "png" is not in \code{plotFormat}. Defaults to 150.
#' @param overwrite logical indicating whether to overwrite an existing file. Defaults to \code{FALSE}. If \code{FALSE} and the file exists, the function will output a warning.
#' @importFrom plotscale as.pdf
#' @import grDevices
#' @export
output_plots <- function(
    x, fileDir = "plots", fileStem,
    plotFormat = c("plotscalePdf", "pdf", "png"), width, height, pngRes = 150,
    overwrite = FALSE) {
  if (!dir.exists(fileDir)) dir.create(fileDir)
  
  # do not allow outputting of both plotscale and pdf/png together, as the sizing parameters are very different
  if (("plotscalePdf" %in% plotFormat) & any(c("pdf", "png") %in% plotFormat))
    stop("Cannot output both plotscale and pdf/png at the same time.")
  
  # note that for plotscalePdf, the width and height are for the plot area, not the graphics output area
  if ("plotscalePdf" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".pdf"))
    if (!file.exists(filenameOut) | overwrite) {
      load_packages_with_install("plotscale")
      as.pdf(x, file = filenameOut, width = width, height = height)
    } else warning(paste0("File ", filenameOut, " already exists."))
  }
  
  if ("pdf" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".pdf"))
    if (!file.exists(filenameOut) | overwrite) {
      pdf(filenameOut,
          width = width, height = height)
      print(x)
      invisible(dev.off())
    } else warning(paste0("File ", filenameOut, " already exists."))
  }
  
  if ("png" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".png"))
    if (!file.exists(filenameOut) | overwrite) {
      png(filenameOut,
          width = width, height = height, units = "in", res = pngRes)
      print(x)
      invisible(dev.off())
    } else warning(paste0("File ", filenameOut, " already exists."))
  }
}

#' @rdname output_plots
#' @export
outputPlots <- output_plots