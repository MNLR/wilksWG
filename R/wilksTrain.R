#' @title Train Complete Wilks Model
#' @author M.N. Legasa
#' @param data grid type object from loadeR. It should contain $Data, binary matrix with the data
#'  If 
#' @export

wilksTrain <- function(data, dates = NULL, stations.id = NULL,
                       accuracy = 10000000, max.error = 0.001,
                       max.iter = 20, debug = FALSE,
                       parallelize = FALSE, n.cores = NULL){
  data <- handleData(data, dates = dates, stations.id = stations.id)
  print("Computing transition probabilites...")
  transition.probabilities <- apply(data$data, MARGIN = 2,
                                    transitionProbability,
                                    dates = data$dates)
  print("Done. Computing pairwise correlations...")
  correlations.train <- cor( data$data , use = "complete.obs")
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
       samples = data$data,
       dates = list(start = data$dates[1], end = data$dates[length(data$dates)]),
       stations.id = data$stations.id
  )
  class(tbr) <- "wilks.model"
  return( tbr )
}