#' Add an edge between two nodes in a layer
#'
#' @description Add an edge between two nodes in a layer
#' @usage add_edge.multiplex(multiplex, x, y, layer, weight)
#' @aliases add_edge.mutliplex
#' @param weight : An integer, the weight of the added edge
#' @param layer : Numeric, the layer's id number where the edge will be added
#' @param x,y : Character, the vertex name attribute
#' @param multiplex : The multiplex object.
#' @return Multiplex network object, whith the edge added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' M <- add_edge.multiplex(M,"2","34",2)
#'
#' @export

add_edge.multiplex <- function(multiplex,x,y,layer,weight=1.0){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(!is.numeric(layer)||is.null(get_layer_by_number.multiplex(multiplex,layer)))
    stop(paste("Illegal layer number : ",toString(layer),sep=""))
  if(is.character(x)&&is.character(y)){
    if((x %in% multiplex@nodes)&& (y %in% multiplex@nodes)){
      if(!are.connected(graph = get_layer_by_number.multiplex(multiplex,layer),v1 = x,v2 = y)){
        multiplex@layers[[layer]] <- add.edges(multiplex@layers[[layer]],edges = c(x,y))
      }
      return(multiplex)
    }
    else stop("Unknown vertices")
  }
  else
    stop("x and y should be a string")

}


#'  Add a list of edges in a layer
#'
#' @description Add a list of edges in a layer of a multiplex network
#' @usage add_edges.multiplex(multiplex, edges, layer)
#' @aliases add_edges.mutliplex
#' @param edges : A list of list, can be a list of tuples(x,y) or tiplets(x,y,w)
#' @param layer : Numeric, the layer's id number where the edge will be added
#' @param multiplex : The multiplex object.
#' @return Multiplex network object, whith the list of edges added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' add_edges.multiplex(M,list(c("1","2"),c("2","3")),3)
#' @export

add_edges.multiplex <- function(multiplex, edges,layer){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.list(edges)){
    for( e in edges){
      if(length(e)==3)
        multiplex <- add_edge.multiplex(multiplex,e[1],e[2],layer,weight=e[3])
      else if(length(e)==2)
        multiplex <- add_edge.multiplex(multiplex,e[1],e[2],layer)
    }
    return(multiplex)
  }
  else
    stop("Illegal type of edges : should be a list of edges (tuples or triplets)")
}
