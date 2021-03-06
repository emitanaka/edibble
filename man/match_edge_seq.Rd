% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nest.R
\name{match_edge_seq}
\alias{match_edge_seq}
\title{Make an edge sequence from two vectors of vertex names}
\usage{
match_edge_seq(.data, vnames_from, vnames_to)
}
\arguments{
\item{.data}{An edibble graph.}

\item{vnames_from}{The vertex name to start the edge.}

\item{vnames_to}{The vertex name to end the edge.}
}
\value{
An edge sequence.
}
\description{
For a pair of vectors of vertex names in \code{.data}, get the edge sequence
that connects the vertices in the matching index. For example, if the graph
has 4 nodes named A, B, C and D, then \code{vnames_from = C("A", "B")} and
\code{vnames_to = c("C", "D")} means it returns the edge sequence so that node A
will connect to node C and node B to node D.
}
\seealso{
Other edge-seq: 
\code{\link{cross_edge_seq}()}
}
\concept{edge-seq}
