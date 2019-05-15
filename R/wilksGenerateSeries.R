#' @title Generate Series from Wilks Weather Generator
#' @author M.N. Legasa
#' @export

wilksGenerateSeries <- function(wilks, n, initial = NULL,
                                initial.date = NULL,
                                tol = 10^(-2)){

  if (is.null(initial.date)) initial.date <- wilks$dates$start
  tbr <-  seriesGenerator(n, wilks$normal.covariances,
                          wilks$transition.probabilities,
                          initial = initial, initial.date = initial.date,
                          tol = tol, samples = wilks$samples
  )
  if (!(is.null(wilks$stations.id))) colnames(tbr) <- wilks$stations.id
  return(tbr)
}


seriesGenerator <- function(n, normal.covariances,
                            transition.probabilities,
                            initial = NULL, 
                            initial.date = NULL, 
                            tol = 10^-6,
                            samples = NULL){
  series <- pnorm(mvrnorm(n, Sigma=normal.covariances,
                          mu = rep(0,ncol(transition.probabilities)), tol = tol
  )
  )
  if (is.null(initial)){
    if (!is.null(samples)){
      initial <- toOperableMatrix(samples[ sample(1:nrow(samples), size = 1), ])
      colnames(series) <- colnames(initial)
    } else{
      initial <- sample(c(0,1), size = ncol(transition.probabilities), replace = TRUE)
    }
  }
  
  series <- rbind(initial, series)
  if (!is.null(initial.date)){
    rownames(series) <- as.character(
      as.Date(initial.date) + seq(0,(nrow(series)-1))
    )
    
  }
  
  
  for (i in 2:(n+1)){
    #print("previous")
    #print(vars[i-1,])
    #print("Random number")
    #print(vars[i, ])
    for (station in 1:ncol(transition.probabilities)){
      if (series[i-1, station] == 0){ #previous is dry
        if (series[i, station] <= transition.probabilities[1, station] ){
          series[i, station] <- 1
        } else { series[i, station] <- 0 }
      }
      else {                  #previous is wet
        if (series[i,station] <= transition.probabilities[2, station]){
          series[i,station] <- 1
        } else { series[i, station] <- 0 }
      }
      
    }
  }
  return(series)
}

