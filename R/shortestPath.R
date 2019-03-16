#' Shortest paths between two vertices in the multiplex network
#'
#' @description Shortest paths between vertices in the multiplex network. The shortest paths will be calculated in a flatten graph. The shortest path length from a vertex to itself is always zero. For unreachable vertices Inf is included
#' @usage shortestPath.multiplex(multiplex,nodei,nodej,flatten_function)
#' @param flatten_function : String, the name of the flatten function
#' @param nodej : Numeric vector, the vertex to which the shortest paths will be calculated
#' @param nodei : Numeric vector, the vertex from which the shortest paths will be calculated.
#' @param multiplex : The multiplex object.
#' @return Numeric scalar
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' shortestPath.multiplex(M,"1","34",flatten_function = flatten_binary)
#' shortestPath.multiplex(M,"1","34",flatten_function = flatten_redundancy)
#' @export
shortestPath.multiplex <- function(multiplex,nodei,nodej,flatten_function=flatten_binary){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(nodei)&&is.character(nodej)&&(nodei%in%multiplex@nodes)&&(nodej%in%multiplex@nodes)){
    flatten_graph = flatten_function(multiplex)
    return(shortest.paths(flatten_graph,v = nodei,to = nodej,weights = get.edge.attribute(flatten_graph,"weight"),mode = "all",algorithm = "dijkstra")[1][1])
  }
  else
    stop("Unknown nodes")
}


#' Shortest paths between all vertices in the multiplex network
#'
#' @description Shortest paths between vertices in the multiplex network. The shortest paths will be calculated in a flatten graph. The shortest path length from a vertex to itself is always zero. For unreachable vertices Inf is included
#' @usage shortestPath_all.multiplex(multiplex,flatten_function)
#' @param flatten_function : String, the name of the flatten function
#' @param multiplex : The multiplex object.
#' @return Numeric square matrix with length of multiplex nodes
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' shortestPath_all.multiplex(M,flatten_function = flatten_binary)
#' shortestPath_all.multiplex(M,flatten_function = flatten_redundancy)
#' @export
shortestPath_all.multiplex <- function(multiplex,flatten_function=flatten_binary){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  flatten_graph = flatten_function(multiplex)
  return(shortest.paths(flatten_graph,weights = get.edge.attribute(flatten_graph,"weight"),mode = "all",algorithm = "dijkstra"))
}


#' Average shortest paths between vertices over all layers in the multiplex network
#'
#' @description Average shortest paths between vertices over all layers in the multiplex network. The shortest path length from a vertex to itself is always zero. For unreachable vertices Inf is included
#' @usage shortestPath_mean.multiplex(multiplex,nodei,nodej)
#' @param nodej : Numeric vector, the vertex to which the shortest paths will be calculated
#' @param nodei : Numeric vector, the vertex from which the shortest paths will be calculated.
#' @param multiplex : The multiplex object.
#' @return Numeric scalar
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' shortestPath_mean.multiplex(M,"1","34")
#' @export
shortestPath_mean.multiplex <- function(multiplex,nodei,nodej){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  spl = 0
  nbLayers = get_number_of_layers.multiplex(multiplex)
  if(is.character(nodei)&&is.character(nodej)&& (nbLayers>0)&&(nodei%in%multiplex@nodes)&&(nodej%in%multiplex@nodes)){
    for(layer in multiplex@layers){
      aux = shortest.paths(layer,v = nodei,to = nodej,weights = get.edge.attribute(layer,"weight"),mode = "all",algorithm = "dijkstra")[1][1]
      if(is.finite(aux)){
        spl = spl + aux
      }
      else
        return(aux)
    }
    return((spl/nbLayers))
  }
  else
    stop("no layer found or unknon nodes")
}

#' Average shortest paths between all vertices over all layers in the multiplex network
#'
#' @description Average shortest paths between all vertices over all layers in the multiplex network. The shortest path length from a vertex to itself is always zero. For unreachable vertices Inf is included
#' @usage shortestPath_mean_all.multiplex(multiplex)
#' @param multiplex : The multiplex object.
#' @return Numeric square matrix with length of multiplex nodes
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' shortestPath_mean_all.multiplex(M)
#' @export
shortestPath_mean_all.multiplex <- function(multiplex){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  spl = 0
  nbLayers = get_number_of_layers.multiplex(multiplex)
  if((nbLayers>0)){
    for(layer in multiplex@layers){
      d <- diameter(layer)
      aux = shortest.paths(layer,weights = get.edge.attribute(layer,"weight"),mode = "all",algorithm = "dijkstra")
      aux[aux==Inf] <-d
      aux <- aux/d
      spl = spl + aux

    }
    return(round(spl/nbLayers,2))
  }
  else
    stop("no layer found or unknon nodes")
}
