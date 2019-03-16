


#'  Multiplex subgraph
#'
#' @description Creates a multiplex subgraph, containing only the specified vertices and all the edges among them. If the vertex name is not the multiplex network the node will not be considered
#' @usage  subgraph.multiplex(multiplex,node_set)
#' @param node_set : Numeric vector, the vertices of the original multiplex which will form the subgraph.
#' @param multiplex : The orginal multiplex object.
#' @return A new multiplex object
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' SubM <- subgraph.multiplex(M,c("1","2","3"))
#' multiplex.summary(SubM)
#' @export
subgraph.multiplex <- function(multiplex, node_set){
  #returns a multiplex subgraph defiend over the node set.
  #the node set is a list of nodes names
  #if the name is not the graph the node will not be considered
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(node_set) && (length(node_set)>0) && all(node_set %in% multiplex@nodes)){
    aux <- new("Multiplex")
    aux <- add_vertices.multiplex(aux,node_set)
    len <- get_number_of_layers.multiplex(multiplex)
    if(len>0){
      for(l in seq(len)){
        g <- simplify(induced.subgraph(get_layer_by_number.multiplex(multiplex,l),vids = node_set),remove.multiple = TRUE,remove.loops = TRUE)
        aux <- add_layer(aux,g)
      }
      return(aux)
    }
    else
      stop("There is no layers in the multiplex")

  }
  else{
    stop("node_set should be a list of existing node where nodes are string ")
  }
}
