% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighbors.R
\name{neighbors_similarity.multiplex}
\alias{neighbors_similarity.multiplex}
\title{Neighboring vertices in the multiplex network, a similarity-guided definition}
\usage{
neighbors_similarity.multiplex(multiplex,node_name,delta,similarity_function)
}
\arguments{
\item{multiplex}{: The multiplex object.}

\item{node_name}{: Character scalar, the vertex name of which the neighbors vertices are queried.}

\item{delta}{: Real scalar, the minimum pourcentage of similarity between . Should be in [0,1].}

\item{similarity_function}{: Character scalar, the name of the similarity function.}
}
\value{
A vertex sequence containing the neighbors of the input vertex
}
\description{
A vertex is a neighbor of another one  in the multiplex network if they have a similarity exceeding a given threshold delta
}
\examples{
M <- Multiplex_Lazega
neighbors_similarity.multiplex(M,node_name ="2", delta=0.5,similarity_function=similarity.jaccard)
neighbors_similarity.multiplex(M,node_name ="2", delta=0.5,similarity_function=similarity.dice)
}
\author{
Issam Falih <issam.falih@lipn.univ-paris13.fr>
}
