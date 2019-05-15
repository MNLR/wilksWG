#' @title Handle data from climate4R package or free format 
#' @author M.N. Legasa

handleData <- function(data, dates = NULL, stations.id = NULL
){
  if (is.null(dates)){
    if (is.null(data$Dates$start)) {
      message("Warning: No dates were specified, 
               assuming no gaps")
    }
    else dates <- data$Dates$start
  }
  
  if (!is.null(data$Metadata$station_id)) {
      stations.id <- data$Metadata$station_id
  }
  
  if (is.list(data)) data <- data$Data
  
  return(list(data = data, dates = dates, stations.id = stations.id))
  
}