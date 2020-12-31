% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vars.R
\name{add_edibble_vertex}
\alias{add_edibble_vertex}
\alias{add_edibble_vertex.lkbl_nexus}
\alias{add_edibble_vertex.default}
\title{Add an edibble vertex}
\usage{
add_edibble_vertex(.nexus, ...)

\method{add_edibble_vertex}{lkbl_nexus}(.lnexus, name, nexus = NULL, attr)

\method{add_edibble_vertex}{default}(x, name, nexus = NULL, attr)
}
\arguments{
\item{name}{Name of the edibble variable as string.}

\item{nexus}{An edibble nexus with levels converted as list used
for evaluation in any expressions.}

\item{x}{A value that may be a single number, unnamed vector,
a one sided formula or linkabble.}
}
\value{
Returns an evaluated expression.
}
\description{
In edibble, a short hand is used to assign new edibble variables/nodes.
This function converts the short hand to make the minimal edibble node
and returns an edibble nexus with that additional node.
}