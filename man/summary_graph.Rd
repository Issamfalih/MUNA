% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/graph_summary.R
\name{summary_graph}
\alias{summary_graph}
\title{Summary of the graph}
\usage{
summary_graph(graph)
}
\arguments{
\item{graph}{: an igraph graph}
}
\value{
A list of graph description
}
\description{
The summary of the input graph
}
\examples{
g = graph.famous("Zachary")
summary_graph(g)
}
\author{
Issam Falih <issam.falih@lipn.univ-paris13.fr>
}
