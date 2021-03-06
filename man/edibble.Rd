% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/edibble.R
\name{edibble}
\alias{edibble}
\alias{as_edibble}
\title{Make input edibble}
\usage{
edibble(.data, name = NULL, ...)

as_edibble(.data, ...)
}
\arguments{
\item{.data}{A named list of variables or data frame.}

\item{name}{An optional name of the design.}

\item{...}{Passed to \code{new_edibble}.}
}
\description{
If variables are already defined as external data then you can import this
data as edibble. You will need to set what the variables are subsequently
after making it an edibble table by calling on functions like \code{set_units}.
\code{set_trts} and \code{record_vars}.
}
\examples{
# regenerating a new design for lady tasting tea
lady_tasting_tea \%>\%
  edibble(name = "Lady tasting tea") \%>\%
  set_units(cup) \%>\%
  set_trts(first) \%>\%
  restart_design() \%>\%
  allocate_trts(~ cup) \%>\%
  randomise_trts() \%>\%
  serve_table()

skittles \%>\%
  edibble(name = "Nick's skittle experiment") \%>\%
  set_units(person,
            order = nested_in(person)) \%>\%
  set_trts(skittle_type) \%>\%
  allocate_trts()
}
\seealso{
See \code{\link[=start_design]{start_design()}} for initiating design from scratch. When
the variables are made edibble, you can \code{\link[=restart_design]{restart_design()}}.
}
