#' @title Parallelization Cluster Starter
#' @param type Cluster type, either FORK or PSOCK.
#' @param n.cores Number of cores.
#' @param PSOCK.funcExports.list For PSOCK type cluster, named list of functions to be exported
#'   It requires the each function name to be specified in the list names
#' @param PSOCK.varExports.list For PSOCK type cluster, named list of variables to be exported
#'   It requires the each variable name to be specified in the list names
#' @author M.N. Legasa
#' @importFrom parallel detectCores makeCluster

parallelHandler <- function(type, n.cores = NULL,
                            PSOCK.funcExports.list = list(),
                            PSOCK.varExports.list = list(),
                            cl = NULL
                            ){
  
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
    PSOCK.exports <- c(PSOCK.funcExports.list, 
                       PSOCK.varExports.list)
    list2env(PSOCK.exports, envir = environment())
    clusterExport(cl = cl, varlist =  names(PSOCK.exports), 
                  envir = environment())
  }
  print("Cluster good to go.")
  return(cl)
}


#' @title Create Named List
#' @author M.N. Legasa

namedList <- function(...) {
  L <- list(...)
  snm <- sapply(substitute(list(...)),deparse)[-1]
  if (is.null(nm <- names(L))) nm <- snm
  if (any(nonames <- nm=="")) nm[nonames] <- snm[nonames]
  setNames(L,nm)
}