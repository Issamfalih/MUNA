
#' Neighboring vertices in the multiplex network, a similarity-guided definition
#'
#' @description A vertex is a neighbor of another one  in the multiplex network if they have a similarity exceeding a given threshold delta
#' @usage neighbors_similarity.multiplex(multiplex,node_name,delta,similarity_function)
#' @param similarity_function : Character scalar, the name of the similarity function.
#' @param delta : Real scalar, the minimum pourcentage of similarity between . Should be in [0,1].
#' @param node_name : Character scalar, the vertex name of which the neighbors vertices are queried.
#' @param multiplex : The multiplex object.
#' @return A vertex sequence containing the neighbors of the input vertex
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' neighbors_similarity.multiplex(M,node_name ="2", delta=0.5,similarity_function=similarity.jaccard)
#' neighbors_similarity.multiplex(M,node_name ="2", delta=0.5,similarity_function=similarity.dice)
#' @export

neighbors_similarity.multiplex <- function(multiplex,node_name,delta, similarity_function = similarity.jaccard){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(node_name)&&(node_name%in%multiplex@nodes)){
    flatten_graph <- flatten_binary(multiplex)
    node_index = which(vertex.attributes(flatten_graph)$name == node_name)
    neigh=vector()
    for( neighbor  in neighbors(flatten_graph,node_index)){
      if(similarity_function(flatten_graph,c(node_index,neighbor),mode = "all",loops = FALSE)[1,2] >= delta)
        neigh = c(neigh,neighbor)
    }
    res = vector()
    for(x in neigh){
      res <- c(res, get.vertex.attribute(flatten_graph,"name",index = x))
    }
    if(length(res)==0)
      return(NULL)
    return(res)
  }
  else
    stop("Unkown node_name or not a string")
}




#' Neighboring vertices in the multiplex network
#'
#' @description A vertex is a neighbor of another one  in the multiplex network if there are adjacent in delta % layers.
#' @usage neighbors.multiplex(multiplex,node_name, delta=0.5)
#' @param delta : Real scalar, the minimum pourcentage of layers in which the neighbors must be adjacent to the vertex node_name. Should be in [0,1].
#' @param node_name : Character scalar, the vertex name of which the neighbors vertices are queried.
#' @param multiplex : The multiplex object.
#' @return A vertex sequence containing the neighbors of the input vertex
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr> Rushed Kanawati <rushed.kanawati@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' neighbors.multiplex(M,node_name ="2", delta=0.5)
#' neighbors.multiplex(M,node_name ="2", delta=1)
#' @export

neighbors.multiplex <- function(multiplex,node_name,delta=0.5){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(node_name)&&(node_name%in%multiplex@nodes)){

    dfNames = vector()
    dfValue = vector()
    nbLayers = get_number_of_layers.multiplex(multiplex)

    for(layer in multiplex@layers){
      for(v in neighbors(layer,node_name)){
        vname= get.vertex.attribute(layer,"name",index = v)
        index = which(dfNames==vname)
        if(length(index)==0){
          dfNames <- c(dfNames,vname)
          dfValue <- c(dfValue,(1/nbLayers))
        }
        else
          dfValue[index]= dfValue[index] + (1/nbLayers)
      }
    }
    return(dfNames[which(dfValue>=delta)])
    #df <- data.frame(dfNames,dfValue)
    #return(subset(df,dfValue>delta)$dfNames)
  }
  else
    stop("Unkown node_name or not a string")
}
