% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/save_file.R
\name{save_file}
\alias{save_file}
\title{Save files for different types of files and for development work}
\usage{
save_file(
  dataset,
  filename,
  out_folder = c("base_file", "output", "tde", "open_data"),
  type = c("csv", "rds", "xlsx"),
  dev = F,
  overwrite
)
}
\arguments{
\item{dataset}{The object that you want to save.}

\item{filename}{A \code{character} string with name of output file.}

\item{type}{A \code{character} string with name of file extension.
This can be csv, rds or xlsx}

\item{dev}{A \code{character} string. It add a "dev_version" bit to the filename
to be able to identify development work
#' @param overwrite A \code{logical} value. To allow overwriting or not of files}

\item{outfolder}{A \code{character} string with name of destination folder.
This can be "base_file", "tde" or "output".}
}
\description{
Save files for different types of files and for development work.
}
