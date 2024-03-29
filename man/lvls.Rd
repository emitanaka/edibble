% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/attrs.R
\name{lvls}
\alias{lvls}
\title{Setting the traits of the levels}
\usage{
lvls(value = NULL, n = NA_integer_, data = NULL, ...)
}
\arguments{
\item{value}{A vector of the level values.}

\item{n}{The number of replicate (if applicable).}

\item{data}{A list or data frame of the same size as the \code{levels}.}

\item{...}{Name-value pair denoting other level attributes. The value should be the same
length as \code{levels} or a single value.}
}
\value{
An edbl_lvls object.
}
\description{
Setting the traits of the levels
}
\examples{
lvls(c("A", "B"))
}
