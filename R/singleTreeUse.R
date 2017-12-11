#library(randomForest)
#library(reprtree)

#node_list to geneids?

#' @export
classify_by_single_tree <- function(cl, drvo){
  
  traverse<-function(node, drvo, cl, node_list=c()){
    node_list<-c(node_list,node)
    if(drvo[node, "status"]==-1){
      return(list(drvo[node, "prediction"], node_list))
    }else{
    
    if(cl[as.character(drvo[node,"split var"])] > drvo[node, "split point"]){
      new_node<-drvo[node, "right daughter"]
    }else{
      new_node<-drvo[node, "left daughter"]
    }
      traverse(new_node, drvo, cl, node_list)
      
    }
  }
  
  traverse(1,drvo,cl)[[1]]
  
  
}


