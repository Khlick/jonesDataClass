% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jonesDataClass.r
\docType{package}
\name{jonesDataClass}
\alias{jonesDataClass}
\alias{jonesDataClass-package}
\title{jonesDataClass: A package for Jones Calium Imaging data}
\usage{
jonesDataClass(files = "", ...)
}
\arguments{
\item{files}{input file names}

\item{...}{passed to other methods}
}
\description{
jonesDataClass: A package for Jones Calium Imaging data
}
\section{Included Functions}{

  \itemize{
    \item[jonesDatum] Data Class for single file
    \item[jonesDataClass] Data Class for collection of \code{datum}s
    \item[fileImporter] Function that parses excel and image files. 
  }
}

\examples{

\donttest{
## Load test files from package
dataFiles <- list.files(
  path = system.file('extdata',package='jonesDataClass'),
  pattern = '.xlsx$',
  full.names = TRUE
)

## Import contents
dataObj <- jonesDataClass(dataFiles, verbose= TRUE)
}

}
