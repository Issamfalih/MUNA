#' Add an igraph graph as a layer to the multiplex
#'
#' @description Add an igraph graph as a layer to the multiplex network. If there is new nodes in the input graph. These new nodes will be added to the multiplex network.
#' @usage add_layer(multiplex, g)
#' @param multiplex : The multiplex object.
#' @param g : An igraph graph i.e : the inserted layer. Nodes of the added graph should have a "name" attribute. This attribute is used to match already existing nodes.
#' @return The multiplex graph with the layer added
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- new("Multiplex")
#' g1 <- graph.famous("Zachary")
#' g1 <- set.vertex.attribute(g1,name = "name",index = V(g1),value = as.character(V(g1)))
#' M <- add_layer(M,g1)
#' g2 <- graph.ring(30)
#' g2 <- set.vertex.attribute(g2,name = "name",index = V(g2),value = as.character(V(g2)))
#' M <- add_layer(M,g2)
#' @export

add_layer <- function(multiplex, g){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(!is.null(vertex.attributes(g)$name)&&is.igraph(g)){
    nodes = c(V(g)$name)
    if(length(nodes)==vcount(g)){
      nodes_to_add = setdiff(nodes,multiplex@nodes)
      if(length(nodes_to_add)>0){
        multiplex <- add_vertices.multiplex(multiplex,as.character(nodes_to_add))
      }
      new_graph  <- graph.empty(n = 0, directed = FALSE)
      if(is.null(get.graph.attribute(g,name = "name"))){
        new_graph <- set.graph.attribute(new_graph,name = "name",value = paste("layer#",get_number_of_layers.multiplex(multiplex)+1,sep=""))}
      else{
        new_graph <- set.graph.attribute(new_graph,name = "name",value = get.graph.attribute(g,name="name"))
      }

      new_graph <-  add.vertices(new_graph,nv = length(multiplex@nodes))
      new_graph<-set.vertex.attribute(new_graph,name="name",index=V(new_graph),value=multiplex@nodes)
      e = get.edgelist(g,names=TRUE)
      if(nrow(e)>0){
        #for(i in seq(nrow(e))){
#           # if(is.weighted(g))
#           #    new_graph <- add.edges(new_graph,edges = c(x,y))
#           #else
#           new_graph <- add.edges(new_graph,edges = c(which(multiplex@nodes==e[i,][1]),which(multiplex@nodes==e[i,][2])))
          new_graph <- add.edges(new_graph,edges =as.vector(t(e)))
       # }
      }
      multiplex@layers[[get_number_of_layers.multiplex(multiplex)+1]]= new_graph
      return(multiplex)
    }
    else
      stop("Attribute name is not a key")
  }
  else
    stop("Graph must be an igraph graph and must have a name attribute")

}
