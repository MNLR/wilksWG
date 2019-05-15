#' @title Find normal correlation
#' @param r correlation
#' @author M.N. Legasa
#' @export


findNormalCorrelation <- function( r , p01e1, p11e1, p01e2, p11e2,
                                   max.error = 0.001, max.iter = 25,
                                   accuracy = 1000000, 
                                   debug = FALSE){
  nc <- r
  ncmin <- 0
  ncmax <- 1
  error <- 1
  iter <- 0
  cor.unif <- normal2UniformCorrelation( nc , p01e1, p11e1, p01e2, p11e2,
                                         accuracy )
  error <-  cor.unif - r
  if (debug){
    to.print <- matrix(c(r, cor.unif, nc, error), nrow = 1, ncol = 4)
    colnames(to.print) <- c("r","achieved", "nc", "error")
    print(to.print)
  }
  while ( abs(error)  > max.error & iter <= max.iter ){
    if (error < 0) {
      if (nc > ncmin) { ncmin <- nc }
      nc <- ncmin + (ncmax-ncmin)/2
    }
    else {
      if (nc < ncmax) { ncmax <- nc }
      nc <- ncmin + (ncmax-ncmin)/2
    }
    cor.unif <- normal2UniformCorrelation( nc , p01e1, p11e1, p01e2, p11e2, 
                                           accuracy )
    error <-  cor.unif - r
    if (debug){
      to.print[1,] <- c(r, cor.unif, nc, error)
      print(c(r, cor.unif, nc, error))
    }
    iter <- iter + 1
  }
  if (abs(error) > max.error){
    message(paste0("Warning: abs(error) > max.error, 
                   reached iterations limit. Error: ",
                   error)
    )
  }
  return(nc)
}