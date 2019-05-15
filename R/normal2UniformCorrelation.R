#' @title Find Uniform Correlation from Normal Correlation
#' @author M.N. Legasa
#' @export

normal2UniformCorrelation <- function(r, p01e1, p11e1, p01e2, p11e2
                                      , accuracy = 10000000){
  vars <- pnorm(mvrnorm(accuracy, Sigma=cbind(c(1, r), c(r, 1)), 
                        mu = c(0,0)))
  # hist(vars[,1], breaks = 100)
  # hist(vars[,2], breaks = 100)
  vars[1,] <- c(0,0)
  for (i in 2:nrow(vars)){
    #print("previous")
    #print(vars[i-1,])
    #print("Random number")
    #print(vars[i, ])
    if (vars[i-1, 1] == 0){ #previous is dry
      if (vars[i, 1] <= p01e1){
        vars[i,1] <- 1
      } else { vars[i,1] <- 0 }
    }
    else {                  #previous is wet
      if (vars[i,1] <= p11e1){
        vars[i,1] <- 1
      } else { vars[i,1] <- 0 }
    }
    # 2nd station
    if (vars[i-1, 2] == 0){
      if (vars[i, 2] <= p01e2){
        vars[i,2] <- 1
      } else { vars[i,2] <- 0 }
    }
    else {
      if (vars[i, 2] <= p11e2){
        vars[i,2] <- 1
      } else { vars[i,2] <- 0 }
    }
  }
  return( cor(vars[,1], vars[,2] ) )
}
