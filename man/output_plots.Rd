% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/output_plots.R
\name{output_plots}
\alias{output_plots}
\alias{outputPlots}
\title{Output plots with specified format and checking for existing file}
\usage{
output_plots(
  x,
  fileDir = "plots",
  fileStem,
  plotFormat = c("plotscalePdf", "pdf", "png"),
  width,
  height,
  pngRes = 150,
  overwrite = FALSE
)

outputPlots(
  x,
  fileDir = "plots",
  fileStem,
  plotFormat = c("plotscalePdf", "pdf", "png"),
  width,
  height,
  pngRes = 150,
  overwrite = FALSE
)
}
\arguments{
\item{x}{plot object}

\item{fileDir}{directory to save the file. Defaults to "plots", which outputs to a subfolder in the working directory. If the specified directory does not exist, the function will create it.}

\item{fileStem}{the stem of the file name, without the extension}

\item{plotFormat}{character vector of plot formats to output. Can be any combination of "plotscalePdf", "pdf", and "png".}

\item{width}{width of the plot or graphics output in inches; for plotscalePdf, this is the width of the plot area, not the graphics output area}

\item{height}{height of the plot or graphics output; for plotscalePdf, this is the width of the plot area, not the graphics output area}

\item{pngRes}{resolution of the PNG file in dpi. Ignored if "png" is not in \code{plotFormat}. Defaults to 150.}

\item{overwrite}{logical indicating whether to overwrite an existing file. Defaults to \code{FALSE}. If \code{FALSE} and the file exists, the function will output a warning.}
}
\description{
This function outputs plots with a specified format, while allowing checks for whether to overwrite
an existing file. It is basically a wrapper on \code{pdf}, \code{png}, and \code{plotscale::as.pdf}.
}
