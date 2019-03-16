\name{Multiplex_Vickers Chan 7thGraders}
\alias{Multiplex_Vickers_Chan_7thGraders}
\docType{data}
\title{Multiplex Vickers Chan 7th Graders}
\description{
The dataset is collected by Vickers from 29 seventh grade students in a school in Victoria, Australia \cite{Vickers81}. Students were asked to nominate their classmates on a number of tree relationships:  Whom do you get on with in class? Who are your best friends class? Who would you prefer to work with?}
\usage{data("Multiplex_Vickers_Chan_7thGraders")}
\format{
  The format is:
Formal class 'Multiplex' [package "MUNA"] with 3 slots
  ..@ name  : chr "Multiplex"
  ..@ layers:List of 3
  .. ..$ :IGRAPH UN-- 29 126 -- Best-friend
  .. ..$ :IGRAPH UN-- 29 240 -- Get_on_with
  .. ..$ :IGRAPH UN-- 29 152 -- Work_with
  ..@ nodes : chr [1:29] "1" "2" "3" "4" ...
}
\references{
Vickers, M., Chan, S.: Representing Classroom Social Structure. Melbourne: Victoria Institute of Secondary Education (1981)
}
\examples{
data(Multiplex_Vickers_Chan_7thGraders)
str(Multiplex_Vickers_Chan_7thGraders)
}
\keyword{datasets}
