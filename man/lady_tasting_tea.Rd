% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{lady_tasting_tea}
\alias{lady_tasting_tea}
\title{Lady tasting tea}
\format{
An object of class \code{tbl_df} (inherits from \code{tbl}, \code{data.frame}) with 8 rows and 4 columns.
}
\source{
Fisher, Ronald (1935) The Design of Experiments.
}
\usage{
lady_tasting_tea
}
\description{
Lady tasting tea experiment was described in Fisher (1935) to test
the ability of a lady who said she tell whether the tea or milk was added
first to a cup of tea.

The experiment consisted of preparing eight cups of tea, four with milk poured
first and the other four with tea poured first. The lady has been told in
advance that there are four of each kind of preparation.

This data consists of the same experimental structure and result but
the order presented in practice is unknown.
\describe{
\item{cup}{The cup number.}
\item{first}{The cup of tea prepared with milk or tea first.}
\item{guess}{The guess by lady which one was poured first.}
\item{correct}{Whether the lady's guess was correct.}
}
}
\seealso{
Other experimental data: 
\code{\link{skittles}}
}
\concept{experimental data}
\keyword{datasets}
