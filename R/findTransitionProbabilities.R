#' @title Find Transition Probabilities
#' @author M.N. Legasa
#' @export

findTransitionProbabilities <- function(day0, day1) {
  aux <- 2*toOperableMatrix(day1) - toOperableMatrix(day0)
  transition.probabilities <- c()
  for (station in 1:ncol(day1)){
    tbl <- table(aux[ , station])
    p01 <- tbl["2"]/(tbl["2"]+tbl["0"])
    p11 <- tbl["1"]/(tbl["1"]+tbl["-1"])
    transition.probabilities <- cbind(transition.probabilities, c(p01, p11))
  }
  rownames(transition.probabilities) <- c("P01", "P11")
  
  return(transition.probabilities)
}