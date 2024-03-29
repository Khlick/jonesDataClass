% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dataList.R
\docType{class}
\name{dataList-class}
\alias{dataList-class}
\alias{dataList}
\alias{plot,dataList,ANY-method}
\title{dataList}
\usage{
\S4method{plot}{dataList,ANY}(x, y = NULL, xlab = "Time", ylab = "",
  axes = FALSE, ...)
}
\arguments{
\item{x}{dataList object, required}

\item{y}{ANY: best if a vector of ints or names}

\item{xlab}{X-axis label (units will be appended)}

\item{ylab}{Y-axis label (units will be appended)}

\item{axes}{bool, draw with or without axes (FALSE by default)}

\item{...}{for plot: passed to plot.window. otherwise unused.}
}
\description{
Class Definition for Storing formatted data with methods to access data, 
subest traces, append/remove files,

plot
}
\section{Fields}{

\describe{
\item{\code{X}}{vector or matrix}

\item{\code{Y}}{coerced to matrix, if no names present, CELL\%d will be applied}

\item{\code{META}}{stoarage of units and names}
}}


