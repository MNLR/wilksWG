#' @title Convert Valid Object to Operable Matrix
#' @author M.N. Legasa
#' @export


toOperableMatrix <- function(data) {
  if (is.vector(data)) {
    pdata <- matrix(0, nrow = 1, ncol = length(data))
    colnames(pdata) <- names(data)
    pdata[1,] <- as.numeric( as.character(data) )
  }
  else {
    if (is.matrix(data)){
      pdata <- matrix(0, ncol = ncol(data), nrow = nrow(data))
    }
    else if (is.data.frame(data)) {
      pdata <- matrix(0, ncol = ncol(data), nrow = nrow(data))
    }
    else {
      stop("Attempted to convert an unrecognizable object.")
    }
    rownames(pdata) <- rownames(data)
    colnames(pdata) <- colnames(data)
    
    for (i in 1:ncol(pdata)){
      pdata[ ,i] <- as.numeric( as.character( data[ , i] ) )
    }
  }
  
  return(pdata)
}
