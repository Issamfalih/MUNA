\name{Multiplex_CS_Aarhus}
\alias{Multiplex_CS_Aarhus}
\docType{data}
\title{
Multiplex CS Aarhus}
\description{
The multiplex social network consists of five kinds of online and offline relationships (Facebook, Leisure, Work, Co-authorship, Lunch) between the employees (61) of Computer Science department at Aarhus.
}
\usage{data("Multiplex_CS_Aarhus")}
\format{
  The format is:
Formal class 'Multiplex' [package "MUNA"] with 3 slots
  ..@ name  : chr "CS-Aarhus Multiplex Social"
  ..@ layers:List of 5
  .. ..$ :IGRAPH UN-- 61 21 -- coauthor
  .. ..$ :IGRAPH UN-- 61 124 -- facebook
  .. ..$ :IGRAPH UN-- 61 88 -- leisure
  .. ..$ :IGRAPH UN-- 61 193 -- lunch
  .. ..$ :IGRAPH UN-- 61 194 -- work
  ..@ nodes : chr [1:61] "1" "2" "3" "4" ...
}
\references{
 Matteo Magnani, Barbora Micenkova, Luca Rossi - Combinatorial Analysis of Multiple Networks. arXiv:1303.4986 (2013)
}
\examples{
data(Multiplex_CS_Aarhus)
 str(Multiplex_CS_Aarhus)
}
\keyword{datasets}
