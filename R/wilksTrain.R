#' @title Train Complete Wilks Model
#' @author M.N. Legasa
#' @param data grid type object from loadeR. It should be a named list with at least $Data, a binary matrix with the data
#' @param dates A vector of dates corresponding to the \code{data} in case it is a matrix. 
#' It should have length(dates) = nrow(data)
#' @param stations.id A vector of station names corresponding to the \code{data} in case it is a matrix. 
#' It should have length(stations.id) = ncol(data)
#' @param accuracy Number of instances of the series that will be generated during each simulation while finding 
#' normal correlation value
#' @param max.error Iterative process will stop when it reaches this error in correlation between pairs or at
#' \code{max.iter} 
#' @param max.iter Maximum number of iterations during the normal correlation value search.
#' @param debug If set to \code{TRUE} information about the iterative process will be displayed. Does not work when 
#' \code{parallelize} is set to \code{TRUE}
#' @param parallel If set to \code{TRUE}, parallel computation will be used.
#' @param n.cores Number of processes that will be created for parallel computation. Set to n.cores()-1 by default.
#' @param cluster.type Either "PSOCK" (default) or "FORK". Check parallel package for the details
#' @export

wilksTrain <- function(data, dates = NULL, stations.id = NULL,
                       accuracy = 1000000, max.error = 0.01,
                       max.iter = 20, debug = FALSE,
                       parallelize = FALSE, n.cores = NULL, cluster.type = "PSOCK"){
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
                               n.cores = n.cores,
                               cluster.type = cluster.type
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