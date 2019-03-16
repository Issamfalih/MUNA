



#' Centralize a multiplex network according to the multiplex degree of vertices
#'
#' @description s  a  method  for  creating  a  graph  level  centralization  measure  from  the  centrality scores of the vertices. See degree_entropy.multiplex
#' @usage centrality_degree.multiplex(multiplex)
#' @param multiplex : The input multiplex network
#' @return A numeric vector with the multiplex degree scores.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_CS_Aarhus
#' centrality_degree.multiplex(M)
#' @export
centrality_degree.multiplex <- function(multiplex){
  nodesCentrality <- vector(mode = "integer")
  nodesCentrality <- degree_entropy.multiplex(multiplex)/(length(multiplex@nodes)-1)
  names(nodesCentrality) <- multiplex@nodes
  return(round(nodesCentrality,3))
}



#' Centralize a multiplex network according to the multiplex closeness of vertices
#'
#' @description Centralize a multiplex network according to the closeness of vertices. See closeness.multiplex.
#' @usage centrality_closeness.multiplex(multiplex)
#' @param multiplex : The input multiplex network
#' @return A numeric vector with the multiplex  closeness scores.
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_CS_Aarhus
#' centrality_closeness.multiplex(M)
#' @export
centrality_closeness.multiplex <- function(multiplex){
  nodesCentrality <- vector(mode = "integer",length= length(multiplex@nodes))
  names(nodesCentrality) <- multiplex@nodes
  for(layer in multiplex@layers){
    nodesCentrality <- nodesCentrality + closeness(layer)
  }
  nodesCentrality <- (nodesCentrality/length(multiplex@layers))
  names(nodesCentrality) <- multiplex@nodes
  return(nodesCentrality)
}
