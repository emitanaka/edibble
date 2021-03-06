% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classics.R
\name{make_classical}
\alias{make_classical}
\alias{code_classical}
\title{Create a classical named experimental design}
\usage{
make_classical(.name = "", ..., .seed = sample(1000, 1), .output = TRUE)

code_classical(.name = "", .seed = sample(1000, 1), ..., .quiet = FALSE)
}
\arguments{
\item{.name}{The short name of the classical named experimental design. See
under Details for the available named designs.}

\item{...}{Parameters passed into the \verb{prep_classical_*} functions.}

\item{.output}{A logical value to indicate whether all output should be
printed or not or a vector of character (e.g. \code{c("info", "code", "table")}) specifying which of the three
outputs should be printed. Default is TRUE.}

\item{.quiet}{Opposite of \code{.code}. Whether to suppress code output.}
}
\description{
The function \code{make_classical} generates a classical named experimental
design by supplying its short name and prints out, by default:
\itemize{
\item \code{info}: information about the named experimental design,
\item \code{code}: code to create the design using edibble, and
\item \code{table}: an edibble data frame for the generated design.
}

You can find the available short names with \code{find_classical_names()}.
}
\details{
For The available named designs are:
\itemize{
\item "crd": completely randomised design
\item "rcbd": randomised complete block design
\item "split": split plot design
}
}
\examples{
make_classical("crd", n = 50, t = 5)
# if you omit the design parameters then it will use the default
# (which may be random)
make_classical("rcbd")
# if you don't give any short names then it will generate a random one
make_classical()
}
\seealso{
See \code{\link[=find_classical_names]{find_classical_names()}} for finding the short names of the
named experimental designs.
}
