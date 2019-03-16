#' Adds a list  of vertex in the multiplex network
#'
#' @description Adds a list of vertex in the multiplex network
#' @usage add_vertices.multiplex(multiplex,vertices)
#' @param vertices : A list of string. It contains the vertices name attribute to be added.  Each vertex name is not already in the multiplex
#' @param multiplex : The multiplex object.
#' @return The multiplex network object, with the new vertices added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' add_vertices.multiplex(M, c("72","73","76","145"))
#' @export
add_vertices.multiplex <- function(multiplex,vertices){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.vector(vertices) &&(length(vertices)>=0)){
    for(v in vertices){
      multiplex <- add_vertex.multiplex(multiplex,toString(v))
    }
    return(multiplex)
  }
  else
    stop("Illegal vertex name : should be a string")

}



#' Adds a new vertex in the multiplex network
#'
#' @description Adds a new vertex in the multiplex network
#' @usage add_vertex.multiplex(multiplex,vertex_name)
#' @param vertex_name : A string, the vertex name attribute to be added.  This vertex name is not already in the multiplex
#' @param multiplex : The multiplex object.
#' @return The multiplex network object, with the new node added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' M <- add_vertex.multiplex(M, "72")
#' # M <- add_vertex.multiplex(M, "1")
#' @export
add_vertex.multiplex <- function(multiplex,vertex_name){
  # Adds a new vertex whose name attribute is vertex_name identified this is not already in the graph
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(vertex_name)&&!(vertex_name %in% multiplex@nodes)){
    multiplex@nodes <- c(multiplex@nodes, as.character(vertex_name))
    layers = length(multiplex@layers)
    if(layers>0){
      for(i in seq(layers)){

        multiplex@layers[[i]] <- add.vertices(multiplex@layers[[i]],nv = 1)#,attr = vertex_name)
        multiplex@layers[[i]] <-set.vertex.attribute(multiplex@layers[[i]],name="name",index=V(multiplex@layers[[i]]),value=multiplex@nodes)
      }
    }
    return(multiplex)
  }
  else
    stop("Illegal vertex name : should be a string and  vertex name is not already in the graph")
}
