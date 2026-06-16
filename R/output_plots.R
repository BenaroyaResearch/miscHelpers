#' Output plots with specified format and checking for existing file
#'
#' This function outputs plots with a specified format, while allowing checks for whether to overwrite
#' an existing file. It is basically a wrapper on \code{pdf}, \code{png}, \code{plotscale::as.pdf}, and \code{plotscale::as.png},
#' with additional checks for file existence and overwriting.
#' @name output_plots
#' @param x plot object
#' @param fileDir directory to save the file. Defaults to "plots", which outputs to a subfolder in the working directory. If the specified directory does not exist, the function will create it.
#' @param fileStem the stem of the file name, without the extension
#' @param plotFormat character vector of plot formats to output. Can be any combination of "plotscalePdf", "plotscalePng", "pdf", and "png".
#' @param width width of the plot or graphics output in inches; for plotscalePdf and plotscalePng, this is the width of the plot area, not the graphics output area
#' @param height height of the plot or graphics output; for plotscalePdf and plotscalePng, this is the height of the plot area, not the graphics output area
#' @param pngRes resolution of the PNG file in dpi. Ignored if \code{plotFormat} does not include "png" or "plotscalePng". Defaults to 150.
#' @param overwrite logical indicating whether to overwrite an existing file. Defaults to \code{FALSE}. If \code{FALSE} and the file exists, the function will output a warning.
#' @importFrom plotscale as.pdf as.png
#' @import grDevices
#' @export
output_plots <- function(
  x,
  fileDir = "plots",
  fileStem,
  plotFormat = c("plotscalePdf", "plotscalePng", "pdf", "png"),
  width,
  height,
  pngRes = 150,
  overwrite = FALSE
) {
  validPlotFormats <- c("plotscalePdf", "plotscalePng", "pdf", "png")

  # ---- input checks ------------------------------------------------------
  # Collect all problems and report them together, rather than failing on
  # the first one, so the caller sees everything that is wrong at once.
  coll <- checkmate::makeAssertCollection()

  # Required arguments with no default: give a clear message if absent.
  if (missing(x)) {
    coll$push("'x' is missing: you must supply a plot object to output.")
  }
  if (missing(fileStem)) {
    coll$push(
      "'fileStem' is missing: you must supply a file name stem (without extension)."
    )
  } else {
    checkmate::assert_string(fileStem, min.chars = 1, add = coll)
  }
  if (missing(width)) {
    coll$push("'width' is missing: you must supply a plot width in inches.")
  } else {
    # lower is inclusive; require a strictly positive width.
    checkmate::assert_number(width, lower = .Machine$double.eps, finite = TRUE, add = coll)
  if (missing(height)) {
    coll$push("'height' is missing: you must supply a plot height in inches.")
  } else {
    checkmate::assert_number(height, lower = .Machine$double.eps, finite = TRUE, add = coll)
  }

  # Arguments with defaults: validate type/shape.
  checkmate::assert_string(fileDir, min.chars = 1, add = coll)
  checkmate::assert_character(
    plotFormat,
    min.len = 1,
    any.missing = FALSE,
    add = coll
  )
  checkmate::assert_subset(plotFormat, choices = validPlotFormats, add = coll)
  checkmate::assert_number(pngRes, lower = 1, finite = TRUE, add = coll)
  checkmate::assert_flag(overwrite, add = coll)

  checkmate::reportAssertions(coll)

  # Mutual exclusivity is a relationship between values, not a per-argument
  # type check, so it stays as an explicit condition.
  if (
    ("plotscalePdf" %in% plotFormat || "plotscalePng" %in% plotFormat) &&
      any(c("pdf", "png") %in% plotFormat)
  ) {
    stop("Cannot output both plotscale and pdf/png at the same time.")
  }
  # ------------------------------------------------------------------------

  if (!dir.exists(fileDir)) {
    ok <- dir.create(fileDir, recursive = TRUE, showWarnings = FALSE)
    if (!ok) {
      stop(paste0("Failed to create output directory: ", fileDir))
    }
  }

  # note that for plotscalePdf and plotscalePng, the width and height are for the plot area, not the graphics output area
  if ("plotscalePdf" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".pdf"))
    if (!file.exists(filenameOut) | overwrite) {
      load_packages_with_install("plotscale")
      as.pdf(
        x,
        file = filenameOut,
        width = width,
        height = height
      )
    } else {
      warning(paste0("File ", filenameOut, " already exists."))
    }
  }

  if ("plotscalePng" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".png"))
    if (!file.exists(filenameOut) | overwrite) {
      load_packages_with_install("plotscale")
      as.png(
        x,
        filename = filenameOut,
        width = width,
        height = height,
        res = pngRes
      )
    } else {
      warning(paste0("File ", filenameOut, " already exists."))
    }
  }

  if ("pdf" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".pdf"))
    if (!file.exists(filenameOut) | overwrite) {
      pdf(filenameOut, width = width, height = height)
      print(x)
      invisible(dev.off())
    } else {
      warning(paste0("File ", filenameOut, " already exists."))
    }
  }

  if ("png" %in% plotFormat) {
    filenameOut <- file.path(fileDir, paste0(fileStem, ".png"))
    if (!file.exists(filenameOut) | overwrite) {
      png(
        filenameOut,
        width = width,
        height = height,
        units = "in",
        res = pngRes
      )
      print(x)
      invisible(dev.off())
    } else {
      warning(paste0("File ", filenameOut, " already exists."))
    }
  }
}

#' @rdname output_plots
#' @export
outputPlots <- output_plots
