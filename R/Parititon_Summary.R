
#' Results summary
#'
#' @description A summary result of a given partition for multiplex
#' @usage ResultsSummary(multiplex,partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return returns the results of clusters validation indexes
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <-  community.layer_aggregation(M)
#' ResultsSummary(M,wt)
#' @export


ResultsSummary <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")

  #print(paste0("Multiplex redundency: ",round(redundency(multiplex),3)))
  print(paste0("Multiplex partition redundency: ",round(redundency_partition(multiplex,partition),3)))
  #print(paste0("Multiplex complementarity: ",round(complementarity(multiplex),3)))
  print(paste0("Multiplex partition complementarity: ",round(complementarity_partition(multiplex,partition),3)))
  print(paste0("Multiplex Modularity partition: ",round(modularity_multiplex(multiplex,partition),3)))
  print("Modularity by layer")
  print(modularity_bylayer.multiplex(multiplex,partition))

}
