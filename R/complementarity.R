

#'  The exclusivity of the multiplex network
#'
#' @description This is the number of pairs of nodes that are connected exclusively in one layer. The exclusivity is ration of edges in the community belonging to one mono diemensional to the total number of edges in the community.
#' @usage exclusivity(multiplex)
#' @param multiplex : The multiplex object.
#' @return A  numeric scalar, the exclusivity of the given multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_CS_Aarhus
#' exclusivity(M)
#' @references
#' M. Berlingerio, M. Coscia, and F. Giannotti, Finding and characterizing communities in multidimensional networks, in ASONAM. IEEE Computer Society, 2011, pp. 490-494.
#' @export

exclusivity <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  graph <- flatten_redundancy(multiplex)
  return(round(length(which(get.edge.attribute(graph, name="weight")==1))/ecount(graph),3))
}




#'  Homgeniety of the multiplex network
#'
#' @description The homgeniety mesures how uniform is the distribution of the number of edgesper layer. The idea that the link intra-community must have a uniform distribution among all layers.
#' @usage homgeniety(multiplex)
#' @param multiplex : The multiplex object.
#' @return A  numeric scalar, the homgeniety of the given multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' homgeniety(M)
#' @references
#' M. Berlingerio, M. Coscia, and F. Giannotti, Finding and characterizing communities in multidimensional networks, in ASONAM. IEEE Computer Society, 2011, pp. 490-494.
#' @export
homgeniety <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(length(multiplex@layers)<=1)
    stop("Can not compute homogeneity for mono-dimensional network")
  Pc <- ecount.multiplex(multiplex)
  avg <-  sum(Pc) / length(multiplex@layers)

  for (layer in multiplex@layers)
    sigma <-  ecount(layer) - avg
  sigma <- sigma^2
  if(sqrt(sigma/length(multiplex@layers))==0)
      return(1)
  return(round(1-(sqrt(sigma/length(multiplex@layers))/sqrt((( max(Pc) - min(Pc))^2)/2)),3))
}






#' Complementarity of a multiplex network
#'
#' @description  The complementarity  is defined as a conjunction of three measures : variety, homgeniety, exclusivity
#' @usage complementarity(multiplex)
#' @param multiplex : The multiplex object.
#' @return A  numeric scalar, the complementarity of the multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' complementarity(M)
#' @references
#' M. Berlingerio, M. Coscia, and F. Giannotti, Finding and characterizing communities in multidimensional networks, in ASONAM. IEEE Computer Society, 2011, pp. 490-494.
#' @export
complementarity <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  return(round(homgeniety(multiplex) * exclusivity(multiplex)  *  variety(multiplex),3))
}


#' Partition complementarity of a multiplex network
#'
#' @description this is defined as a conjunction of three measures : variety, homgeniety, exclusivity
#' @usage complementarity_partition(multiplex, partition)
#' @param partition : an igraph  communities object, vertex membership.
#' @param multiplex : The multiplex object.
#' @return A  numeric scalar, the complementarity of the given multiplex network
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' wt <- community.partition_aggregation(M)
#' complementarity_partition(M, wt)
#' @references
#' M. Berlingerio, M. Coscia, and F. Giannotti, Finding and characterizing communities in multidimensional networks, in ASONAM. IEEE Computer Society, 2011, pp. 490-494.
#' @export
complementarity_partition <- function(multiplex, partition){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  compl <- 0
  for(com in partition){
    tmp <- complementarity(subgraph.multiplex(multiplex,as.character(com)))
    if(is.na(tmp))
      tmp <- 0
    compl <- compl + 0
  }
  return(round(compl/length(partition),3))
}




#' Variety of a multiplex network
#'
#' @description This is the proportion of occurrence of the community  across layers of the multiplex.
#' @usage variety(multiplex)
#' @param multiplex : The multiplex object.
#' @return  Numeric scalar, the variety score .
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' variety(M)
#' @references
#' M. Berlingerio, M. Coscia, and F. Giannotti, Finding and characterizing communities in multidimensional networks, in ASONAM. IEEE Computer Society, 2011, pp. 490-494.
#' @export
variety <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  v <- 0
  for(layer in multiplex@layers){
    if(ecount(layer)>0){
      v <- v + 1
    }
  }
  return(round((v-1)/(length(multiplex@layers) - 1),3))
}
