\name{Multiplex_CKM_Physicians_Innovation}
\alias{Multiplex_CKM_Physicians_Innovation}
\docType{data}
\title{Multiplex CKM Physicians Innovation}
\description{
 This dataset describes relationships among physicians in the context of new drug adoption. Observed relationships include: advice, discussion and friendship}
\usage{data("Multiplex_CKM_Physicians_Innovation")}
\format{
  The format is:
Formal class 'Multiplex' [package "MUNA"] with 3 slots
  ..@ name  : chr " CKM Physicians Innovation Multiplex"
  ..@ layers:List of 3
  .. ..$ :IGRAPH UN-- 246 449 -- Advice

  .. ..$ :IGRAPH UN-- 246 498 -- Discussion

  .. ..$ :IGRAPH UN-- 246 423 -- Friend

  ..@ nodes : chr [1:246] "1" "2" "3" "4" ...
}
\references{
Coleman, J., Katz, E., Menzel, H.: The discusion of an innovation among physicians. Sociometry 20, 253-270 (1957)
}
\examples{
data(Multiplex_CKM_Physicians_Innovation)
multiplex.summary(Multiplex_CKM_Physicians_Innovation)
}
\keyword{datasets
}
