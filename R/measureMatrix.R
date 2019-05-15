#' @title Measure Matrix
#' @param type data
#' @author M.N. Legasa
#' @export

measureMatrix <- function( data , measure = "phiCoef", 
                           mark.diagonal.na = TRUE, ... ){
  if (class(data) == "measureMatrix"){
    return(data)
  }
  else{
    if (is.matrix(data) | is.data.frame(data)){
      data <- list(Data = data)
    }
    mS <- matrix(0, nrow = ncol(data$Data), ncol = ncol(data$Data))
    for (i in 1:nrow(mS)){
      mS[i, ] <- measureVector(data = data, station = i, 
                               measure = measure, ...)
    }
    if (mark.diagonal.na){
      for (i in 1:nrow(mS)){
        mS[i,i] <- NA
      }
    }
    colnames(mS) <- data$Metadata$station_id
    rownames(mS) <- data$Metadata$station_id
    class(mS) <- "measureMatrix"
    return(mS)
  }
}

measureVector <- function(data, station, measure = "phiCoef", ... ){  #remove.na = TRUE, evidence.nodes = NULL, evidence = NULL) {
  
  if (is.matrix(data) | is.data.frame(data)){
    nstations <- ncol(data)
  } else {
    nstations <- ncol(data$Data)
  }
  
  dS <- c()
  casesS <- c()
  
  for (j in 1:nstations){
    d <- do.call(measure, list(data = data, st1 = station, st2 = j, ...) )
    dS[j] <- d
    casesS[j] <- attributes(d)$cases
  }
  attr(dS, "cases") <- casesS[1]
  attr(dS, "measure") <- measure
  return(dS)
}


phiCoef <- function( data, st1, st2, remove.na =  TRUE, evidence.nodes = NULL, evidence = NULL ){
  
  if (length(evidence.nodes) != length(evidence)) {stop("Provide a single evidence for every node.")}
  Data <- filterData(data, st1, st2, remove.na, evidence.nodes, evidence)
  
  ## Phi coefficient equals Pearson for binary variables.
  if (nrow(Data) <= 1) {stop("Less than 1 case matched the evidence.")}
  n11 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n11)) {n11 <- 0}
  n00 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n00)) {n00 <- 0}
  n10 <- as.numeric(table(Data[ ,1] == 1 & Data[ ,2] == 0)["TRUE"])
  if (is.na(n10)) {n10 <- 0}
  n01 <- as.numeric(table(Data[ ,1] == 0 & Data[ ,2] == 1)["TRUE"])
  if (is.na(n01)) {n01 <- 0}
  
  n1_ <- as.numeric(table(Data[ ,1] == 1)["TRUE"])
  n0_ <- as.numeric(table(Data[ ,1] == 0)["TRUE"])
  n_1 <- as.numeric(table(Data[ ,2] == 1)["TRUE"])
  n_0 <- as.numeric(table(Data[ ,2] == 0)["TRUE"])
  
  phi <- (n11*n00 - n10*n01)/(sqrt(n1_)*sqrt(n0_)*sqrt(n_1)*sqrt(n_0))
  attr(phi, 'cases') <- nrow(Data)
  
  return( phi )
}