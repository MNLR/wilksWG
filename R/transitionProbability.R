#' @title Transition Probability
#' @author M.N. Legasa
#' @export

transitionProbability <- function(ts,  dates){
  dates <- as.Date(dates)
  disc <- diff(dates)
  dates <- NULL
  ind <- if (any(disc > 1)) {
    c(0, which(disc > 1))
  } else {
    c(0, length(ts))
  }
  ind <- c(ind, length(ts))
  
  base_ <- c("-1", "0", "1", "2")
  cosa  <- sapply(1:(length(ind)-1), function(x) {
    aux <- ts[(ind[x] + 1):ind[x+1]]
    aux <- 2*toOperableMatrix(aux[1:(length(aux)-1)]) - 
      toOperableMatrix(aux[2:length(aux)])
    
    tbl <- table(aux)
    missing_ <- base_[setdiff(1:4,match(names(tbl), base_))]
    add_ <- rep(0, length(missing_))
    names(add_) <- missing_
    tbl <- c(tbl, add_)
    tbl <- tbl[match(x = base_ , table = names(tbl))] # makes sure sapply doesn't fuck it up
    return(tbl)
  }
  )
  if (!is.matrix(cosa)) cosa <- as.matrix(cosa)
  
  p01 <- sum(cosa["-1",], na.rm = T)/sum(c(cosa["-1",],cosa["0",]), na.rm = T)
  p11 <- sum(cosa["1",], na.rm = T)/sum(c(cosa["1",], cosa["2",]), na.rm = T)
  if (is.na(p01) || is.na(p11)) {
    warning("One of the transition probabilities is NA. This may be due to
            insufficient data.")
  }
  return(c(p01 = p01, p11 = p11))
}