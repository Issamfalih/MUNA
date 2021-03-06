% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diameter.R
\name{diameter_multiplex}
\alias{diameter_multiplex}
\title{Diameter of a multiplex network.}
\usage{
diameter_multiplex(multiplex)
}
\arguments{
\item{multiplex}{: The multiplex object.}
}
\value{
numeric constant.
}
\description{
The diameter of a multiplex network is the average diameter over each layer.
}
\examples{
M <- Multiplex_Lazega
diameter_multiplex(M)
}
\author{
Issam Falih <issam.falih@lipn.univ-paris13.fr>
}
