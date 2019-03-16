

#'  Degree based entropy of  vertices in the multiplex network
#'
#' @description The degree of vertices in the multiplex network, the number of its similar neighbors ie neighbors_similarity.multiplex
#' @usage degree_entropy.multiplex(multiplex,node_name)
#' @param node_name : Character scalar, or list. The vertex name of which the degree vertices are queried.
#' @param multiplex : The multiplex object.
#' @return numeric vector of the same length as argument node_name
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' degree_entropy.multiplex(M)
#' degree_entropy.multiplex(M,c("2","4","45"))
#' @export
degree_entropy.multiplex <- function(multiplex,node_name=NULL){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.null(node_name)||is.vector(node_name)){
    if(is.null(node_name))
      target = multiplex@nodes
    else
      target = node_name
    res = vector()
    flatten_graph <- flatten_binary(multiplex)
    for(v in target){
        degree_v =0
        degree_v_tot <- degree(flatten_graph,v = v)[[1]]
        for(layer in multiplex@layers){
          if(degree_v_tot==0){
            degree_v =0
            break
          }
          else{
            t = ((degree(layer,v=v)[[1]])/degree_v_tot)
            if(t!=0)
              degree_v=degree_v + ( t * log2(t))
            else
              degree_v = 0
          }
        }
        res  <- c( res, round(-degree_v,2))
      }
    names(res) = target
    return(round(res,3))
  }
  else if(is.character(node_name)&&(length(node_name)==1)){
    flatten_graph <- flatten_binary(multiplex)
    degree_v =0
    degree_v_tot <- degree(flatten_graph,v = node_name)[[1]]
    for(layer in multiplex@layers){
      if(degree_v_tot==0){
        degree_v =0
        break
      }
      else{
        t = ((degree(layer,v=node_name)[[1]])/degree_v_tot)
        if(t!=0)
          degree_v=degree_v + ( t * log2(t))
        else
          degree_v = 0
      }
    }
    return(round(-degree_v,3))
  }
  else
    stop("Error node_name should be a list or a string node name")
}




#' Degree of vertices in the multiplex network
#'
#' @description The degree of vertices in the multiplex network based on the cardinality of the union/intersect neighborhood.
#' @usage degree.multiplex(multiplex,node_name,delta)
#' @param delta : Numeric scalar, the minimum pourcentage of layers in which the neighbors must be adjacent to the vertex.
#' @param node_name : Character scalar, or list. The vertex name of which the degree vertices are queried.
#' @param multiplex : The multiplex object.
#' @return numeric vector of the same length as argument node_name
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' degree.multiplex(M, delta=0.3)
#' degree.multiplex(M,"2",0.3)
#' degree.multiplex(M,c("2","5","45"),0.3)
#' @export


degree.multiplex <- function(multiplex,node_name=NULL,delta=0.5){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.character(node_name)&&(length(node_name)==1)){
    return(length(neighbors.multiplex(multiplex,node_name,delta)))
  }
  else if(is.null(node_name)||is.vector(node_name)){
    degree_list = vector()
    if(is.null(node_name))
      target = multiplex@nodes
    else
      target = node_name
    for(v in target){
      if(is.character(v)&&(v%in%multiplex@nodes)){
        neighbor = neighbors.multiplex(multiplex,v,delta=delta)
        degree_list = c(degree_list,length(neighbor))
      }
    }
    names(degree_list) = target
    return(degree_list)
  }
  else
    stop("Error node_name should be a list or a string node name")
}


#'  Degree based similarity of  vertices in the multiplex network
#'
#' @description The degree of vertices in the multiplex network, the number of its similar neighbors ie neighbors_similarity.multiplex
#' @usage degree_similarity.multiplex(multiplex,node_name,similarity_function,delta)
#' @param delta : Numeric scalar, the minimum pourcentage of layers in which the neighbors must be adjacent to the vertex.
#' @param similarity_function : Character scalar, the name of the similarity function.
#' @param node_name : Character scalar, or list. The vertex name of which the degree vertices are queried.
#' @param multiplex : The multiplex object.
#' @return Dataframe the name of  vertices and the degree of each vertex.
#' @author
#' Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' degree_similarity.multiplex(M,"2" ,similarity.jaccard, 0)
#' degree_similarity.multiplex(M,c("2","4","45"), similarity.jaccard, 0.3)
#' degree_similarity.multiplex(M, similarity_function = similarity.jaccard, delta=0.8)
#' degree_similarity.multiplex(M, similarity_function = similarity.invlogweighted,delta= 0.3)
#' degree_similarity.multiplex(M, similarity_function = similarity.dice, delta=0.3)
#' @export


degree_similarity.multiplex <- function(multiplex,node_name=NULL,similarity_function = similarity.jaccard,delta=0.5){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
  if(is.null(node_name)||is.vector(node_name)){
    flatten_graph <- flatten_binary(multiplex)
    dfNames = vector()
    dfValue = vector()
    if(is.null(node_name))
      target = multiplex@nodes
    else
      target = node_name
    for(vname in target){
      if(is.character(vname)&&(vname%in%multiplex@nodes)){
        dfNames <- c(dfNames,vname)
        dfValue <- c(dfValue, (length(which(similarity_function(flatten_graph,c(vname,neighbors(flatten_graph,vname)),mode = "all")[1,] >= delta))-1))
        }
    }
    names(dfValue) <- dfNames
    return(dfValue)
  }
  else if(is.character(node_name)&&(length(node_name)==1)&&(node_name%in%multiplex@nodes)){
    return(length(neighbors_similarity.multiplex(multiplex,node_name,delta,similarity_function)))
  }
  else
    stop("Error node_name should be a list or a string node name")
}



#' Average segree of vertices in the multiplex network
#'
#' @description The average degree of vertices across all layers in the multiplex network.
#' @usage degree_average.multiplex(multiplex,node_name)
#' @param node_name : Character scalar, or list. The vertex name of which the degree vertices are queried.
#' @param multiplex : The multiplex object.
#' @return numeric vector of the same length as argument node_name
#' @author  Issam Falih <issam.falih@lipn.univ-paris13.fr>
#' @examples
#' M <- Multiplex_Lazega
#' degree_average.multiplex(M)
#' degree_average.multiplex(M,"2")
#' @export


degree_average.multiplex <- function(multiplex,node_name=NULL){
  if(!is.multiplex(multiplex))
    stop("Is not a multiplex network")
    if(is.null(node_name))
      target = multiplex@nodes
    else
      target = node_name
    degree_list = vector("integer", length(target))
    names(degree_list) = target
    for(layer in multiplex@layers){
    for(v in target){
      if(is.character(v)&&(v%in%multiplex@nodes)){
        degree_list[as.character(v)] = degree_list[as.character(v)] + length(neighbors(layer,as.character(v)))
      }
      else
        stop("node_name should be in the multiplex network")
    }
    }
    return(degree_list/length(multiplex@layers))
}
