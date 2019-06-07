#' @title Filter Data according to condition evidence on evidence.nodes
#' @param type data
#' @author M.N. Legasa



filterData <- function(data, st1, st2, remove.na, evidence.nodes,
                       evidence, silent = TRUE){
  if (is.list(data)){
    Data <- data$Data
    node.names <- data$Metadata$station_id
  } else {
    Data <- data
    node.names <- NULL
  }
  
  if (is.character(evidence.nodes) && is.null(node.names) ){
    stop("data$Metadata$station_id not found. Please provide evidence nodes as number.")
  }
  
  N0 <- nrow(Data)
  
  if (is.character(st1)){ st1<- which(node.names == st1) }
  if (is.character(st2)){ st2<- which(node.names == st2) }
  if (!is.null(evidence.nodes)){
    aux.evidence.nodes <- rep(0,length(evidence.nodes))
    for (i in 1:length(evidence.nodes)){
      if (is.character(evidence.nodes[i])){
        aux.evidence.nodes[i] <- which(node.names == evidence.nodes[i])
      } else { aux.evidence.nodes[i] <- evidence.nodes[i]}
    }
    evidence.nodes <- aux.evidence.nodes
  }
  
  if (remove.na){ Data <- Data[complete.cases(Data), ] }
  
  if (!is.null(evidence.nodes)){
    for (i in 1:length(evidence.nodes)){
      Data <- Data[ which(Data[ , evidence.nodes[i]] == evidence[i] ) , ]
    }
  }
  
  Data <- Data[ , c(st1, st2)]
  
  N <- nrow(Data)
  if (!silent){
    print(paste0(N, " observations remaining."))
  }
  return(Data)
}