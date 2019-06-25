#' @title Parallelization Cluster Starter
#' @param type Cluster type, either FORK or PSOCK.
#' @param n.cores Number of cores.
#' @param PSOCK.Exports.list For PSOCK type cluster, named list of
#'  functions to be exported. It requires each function and variable name name to be 
#'   specified in the list names. Use \Code{namedList()} if necessary.
#' @author M.N. Legasa
#' @importFrom parallel detectCores makeCluster

parallelHandler <- function(parallelize = FALSE, 
                            type = "PSOCK", n.cores = NULL,
                            cl = NULL,
                            PSOCK.Exports.list = list()
                            ){
  if (parallelize){
    if (is.null(cl)){   # Initiate cluster, if not already
      if ( is.null(n.cores) ){
        n.cores <- detectCores()-1
      }
      print(paste0("Starting cluster of type ", type , " (", n.cores,
                   " threads) for parallel computation..."))
      cl <- makeCluster( n.cores, type = type )
    }
  
    if (type == "PSOCK") {
      print("Exporting data to PSOCK cluster...")
      list2env(PSOCK.Exports.list, envir = environment())
      clusterExport(cl = cl, varlist =  names(PSOCK.Exports.list), 
                    envir = environment())
    }
    print("Cluster good to go.")
  } else cl <- NULL
  return(cl)
}


#' @title Create Named List
#' @author M.N. Legasa

namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)), deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}