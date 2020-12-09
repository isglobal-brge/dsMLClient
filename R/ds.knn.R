#' @title K-Nearest Neighbour Classification
#' 
#' @description Perform a non-disclosive distributed K-Nearest Neighbour Classification
#'
#' @param x \code{character} Name of the data frame on the server that has the dataset with a column that contains
#' the classification objective
#' @param classificator \code{character} Name of column on the table 'x' that has the classifier factor
#' @param query \code{numeric} vector to be queried on the dataset to be classified, the length has to match
#' the number of columns on the 'x' data frame minus one (the classification column is not included on the query obviously)
#' @param neigh \code{numeric} (default \code{3}) number of neighbours considered
#' @param method.indicator \code{character} (default \code{"knn"}) specifies the method that is used to
#' generated non-disclosive coordinates to calculate the euclidean distance. This argument can be set as \code{'knn'}
#'  or \code{'noise'}. For more information see Details.
#' @param k \code{numeric} (default \code{3}) he number of the nearest neighbors for which their centroid is calculated. 
#' For more information see Details.
#' @param noise \code{numeric} (default \code{0.25}) the percentage of the initial variance that is used as the variance 
#' of the embedded noise if the argument method is set to \code{'noise'}. For more information see Details.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#' 
#' @details 
#' If the argument \code{method} is set to \code{'knn'}, the server-side function searches
#' for the \code{k-1} nearest neighbors of each single data point and calculates the centroid 
#' of such \code{k} points. 
#' The proximity is defined by the minimum Euclidean distances of z-score transformed data.
#' 
#' When the coordinates of all centroids are estimated the function applies scaling to expand the
#' centroids back to the dispersion of the original data. The scaling is achieved by multiplying
#' the centroids with a scaling factor that is equal to the ratio between the standard deviation of
#' the original variable and the standard deviation of the calculated centroids. The coordinates of
#' the scaled centroids are then returned to the client-side.
#' 
#' The value of \code{k} is specified by the user. 
#' The suggested and default value is equal to 3 which is also
#' the suggested minimum threshold that is used to prevent disclosure which is specified in the
#' protection filter \code{nfilter.kNN}. When the value of \code{k} increases, 
#' the disclosure risk decreases but the utility loss increases.
#' The value of \code{k} is used only
#' if the argument \code{method} is set to \code{'knn'}. 
#' Any value of \code{k} is ignored if the
#' argument \code{method} is set to \code{'noise'}.
#' 
#' If the argument \code{method} is set to \code{'noise'}, 
#' the server-side function generates a random normal noise of zero mean
#' and variance equal to 10\% of the variance of each \code{x} and \code{y} variable.
#' The noise is added to each \code{x} and \code{y} variable and the disturbed by the addition of
#' \code{noise} data are returned to the client-side. Note that the seed random number generator is fixed to a
#' specific number generated from the data and therefore the user gets the same figure every time
#' that chooses the knn method in a given set of variables.
#' The value of \code{noise} is used only if the argument \code{method} is set to \code{'noise'}.
#' Any value of \code{noise} is ignored if
#' the argument \code{method} is set to \code{'knn'}. 
#'
#' @return \code{character} with the classification assigned to the queried vector
#' @export

ds.knn <- function(x, classificator, query, neigh = 3, method.indicator = "knn", k = 3, noise = 0.25, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if((length(ds.colnames(x)[[1]]) - 1) != length(query)){
    stop(paste0("Length of 'query' [", length(query), 
                "] is longer than variables on 'x' dataset [", (length(ds.colnames(x)[[1]]) - 1), "]"))
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  
  if(!any(cls %in% c("data.frame"))){
    stop("The input vector is not of class 'data.frame'")
  }
  
  cally <- paste0("knnDS(", x, ", ", neigh, ", '", classificator, "', '", method.indicator, "', ", k, ", ", noise, ", ", paste0(query, collapse=","), ")")
  results <- DSI::datashield.aggregate(datasources, as.symbol(cally))

  # Order results (distance and tags from all the servers), get the most seen tag and return it
  results <- lapply(results, function(x) cbind(distance = x$distance, classification = as.character(x$classification)))
  results <- data.frame(do.call("rbind", results))
  results$distance <- as.numeric(results$distance)
  data.table::setorder(results, distance)
  results <- table(results[1:neigh, 2])
  
  return(names(results[1]))
  
}
