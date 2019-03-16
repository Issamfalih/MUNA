\name{Multiplex_Lazega}
\alias{Multiplex_Lazega}
\docType{data}
\title{
Multiplex Lazega}
\description{
This dataset comes from a network study of corporate law partnership. The network describes relationships between employees in gems of three different relationships: co-working, advice and friendship}
\usage{data("Multiplex_Lazega")}
\format{
  The format is:
Formal class 'Multiplex' [package "MUNA"] with 3 slots
  ..@ name  : chr "multiplex"
  ..@ layers:List of 3
  .. ..$ :IGRAPH UN-- 71 559 -- Advice
  .. ..$ :IGRAPH UN-- 71 378 -- Co-work
  .. ..$ :IGRAPH UN-- 71 726 -- Friendship
  ..@ nodes : chr [1:71] "1" "2" "3" "4" ...
}
\references{
Lazega, E.: The Collegial Phenomenon: The Social Mechanisms of Cooperation
Among Peers in a Corporate Law Partnership. Oxford university Press (2001)}
\examples{
data(Multiplex_Lazega)
str(Multiplex_Lazega)
}
\keyword{datasets}
