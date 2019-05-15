#' @title Find Normal Covariance Matrix
#' @author M.N. Legasa
#' @export
#' 
findNormalCovarianceMatrix <- function(correlations, transition.probabilities,
                                       max.error = 0.001, max.iter = 25,
                                       debug = FALSE,
                                       accuracy = 1000000, 
                                       parallelize = TRUE,
                                       cluster.type = "PSOCK",
                                       n.cores = NULL) {
  unraveled <- c(correlations)[c(t(upper.tri(correlations)))]
  
  unraveled.stations.index <-
    paste(t(matrix(rep(as.character(1:nrow(correlations)),
                       nrow(correlations)), nrow = nrow(correlations)
    )),
    matrix(rep(as.character(1:nrow(correlations)),
               nrow(correlations)), nrow = nrow(correlations)
    ), sep = "."
    )
  unraveled.stations.index  <-
    c(unraveled.stations.index)[c(t(upper.tri(correlations)))]
  
  unraveled.stations.index <- strsplit(unraveled.stations.index, ".", fixed = TRUE)
  
  lapply.input <-
    mapply(FUN = function(cor, sts) {return(c(cor, as.numeric(sts)))} ,
           unraveled, unraveled.stations.index , SIMPLIFY = FALSE
    )
  if (parallelize) {
    cl <-
      parallelHandler(type = cluster.type, n.cores = n.cores,
                      PSOCK.funcExports.list = namedList(findNormalCorrelation, 
                                                         mvrnorm),
                      PSOCK.varExports.list = namedList(lapply.input,
                                                        transition.probabilities,
                                                        max.iter,
                                                        max.error,
                                                        accuracy
                      ),
                      cl = NULL
      )
  } else {cl <- NULL}
  print("Finding correlations...")
  ncorrelations <- pbsapply(cl = cl, lapply.input, FUN =  function(input_){
    st <- input_[2:3]
    return(
      findNormalCorrelation(input_[1],
                            p01e1 = transition.probabilities[1,st[1]],
                            p11e1 = transition.probabilities[2,st[1]],
                            p01e2 = transition.probabilities[1,st[2]],
                            p11e2 = transition.probabilities[2,st[2]],
                            max.iter = max.iter,
                            max.error = max.error,
                            accuracy = accuracy,
                            debug = debug
      )
    )
  }
  )
  
  unraveled.i <- 0
  for (i in 1:nrow(correlations)){
    for (j in i:nrow(correlations)) {
      if (i == j) {
        correlations[i, j] <- 1
      }
      else {
        unraveled.i <- unraveled.i + 1
        correlations[i,j] <- ncorrelations[unraveled.i]
        correlations[j,i] <- ncorrelations[unraveled.i]
      }
    }
  }
  
  if (parallelize) {
    stopCluster(cl)
    print("Cluster is off.")
  }
  
  return(correlations)
}
