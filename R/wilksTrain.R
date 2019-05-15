#' @title Train Complete Wilks Model
#' @author M.N. Legasa
#' @export

wilksTrain <- function(data, accuracy = 10000000, max.error = 0.001,
                       max.iter = 20, debug = FALSE,
                       parallelize = FALSE, n.cores = NULL){
  print("Computing transition probabilites...")
  transition.probabilities <- apply(data$Data, MARGIN = 2,
                                    transitionProbability,
                                    dates = data$Dates$start)
  print("Done. Computing pairwise correlations...")
  correlations.train <- measureMatrix( data = data$Data )
  print("Done. Computing Normal covariance matrix...")
  normal.covariances <- 
    findNormalCovarianceMatrix(correlations.train,
                               transition.probabilities,
                               accuracy = accuracy,
                               max.error = max.error,
                               max.iter = max.iter,
                               debug = debug,
                               parallelize = parallelize,
                               n.cores = n.cores
                               )
  print("Done")
  tbr <-
  list(transition.probabilities = transition.probabilities,
       normal.covariances = normal.covariances,
       samples = data$Data
  )
  class(tbr) <- "wilks.model"
  return( tbr )
}