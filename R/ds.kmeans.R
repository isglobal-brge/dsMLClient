#' @title K-Means clustering of distributed table
#' 
#' @description Performs a k-means clustering on a distributed table using euclidean distance
#'
#' @param x \code{character} Name of the data frame on the study server with the data to train the k-means
#' @param k \code{numeric} Integer numeric with the number of clusters to find
#' @param convergence \code{numeric} (default \code{0.001}) Threshold error for the iterations
#' @param max.iter \code{numeric} (default \code{100}) Maxim number of iterations to stop the algorithm
#' @param centroids \code{data frame} (default \code{NULL}) If \code{NULL} random starting centroids will be calculated
#' using the 10/90 inter-quartile range. If a value is supplied, those centroids will be used to start the algorithm. 
#' Structure of the data frame to be supplied: \cr
#' - Each column corresponds to a centroid, so 3 columns correspond to a k-means with k = 3 \cr
#' - Each row corresponds to the value of each variable, this has to match the data frame of name 'x' on the server
#' in both length and order.
#' @param assign \code{bool} (default \code{TRUE}) If \code{TRUE} the results of the cluster will be added to the data frame
#' on the server side
#' @param name \code{character} (default \code{NULL}) If \code{NULL} and \code{assign = TRUE}, the original table 'x'
#' will be overwritten on the server side with an additional column named 'kmeans.cluster' that contain the results
#' of the k-means. If a value is provided on this argument, a new object on the server side will be created with the
#' values from the original table 'x' + the new 'kmeans.cluster' column.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#' 
#' @details This implementation of the kmeans is basically a parallel kmeans where each server acts as a thread. It can be
#' applied because the results that are passed to the master (client) are not disclosive since they are aggregated values 
#' that cannot be traced backwards. The assignations vector is not disclosive since all the information that can be 
#' extracted from it is the same given by the ds.summary function. For more information on the implementation please refer
#' to 'Parallel K-Means Clustering Algorithm on DNA Dataset' by Fazilah Othman, RosniAbdullah, Nur’Aini Abdul Rashid and
#' Rosalina Abdul Salam
#'
#' @return \code{data frame} Where: \cr
#' -Each column corresponds to a centroid (1:k) \cr
#' -Each row corresponds to the a variable of the server data frame
#' @export

ds.kmeans <- function(x, k = NULL, convergence = 0.001, max.iter = 100, centroids = NULL, assign = TRUE, name = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  
  if(!any(cls %in% c("data.frame"))){
    stop("The input vector is not of class 'data.frame'")
  }
  
  if(is.null(k) & is.null(centroids)){
    stop("Please supply a k or centroids to compute the k-means")
  }
  
  # Create a copy of the data frame (with the numerical columns only!)
  ds.subset_type(x, type = "numeric", newobj = paste0(x, "_aux"), datasources)
  
  x <- paste0(x, "_aux")
  
  # Create k centroids
  columns <- ds.colnames(x, datasources)[[1]]
  if(is.null(centroids)){
    # Get 10% and 90% quantiles for each column to create random vectors
    centroids <- matrix(0, ncol = k, nrow = length(columns))
    for(i in 1:length(columns)){
      quants <- ds.summary(paste0(x, "$", columns[i]), datasources)
      min_var <- min(unlist(lapply(quants, function(x) x$`quantiles & mean`[[2]])))
      max_var <- max(unlist(lapply(quants, function(x) x$`quantiles & mean`[[6]])))
      centroids[i,] <- stats::runif(k, min_var, max_var)
    }
  }
  else{k <- ncol(centroids)}
  
  error <- 1
  iter <- 1
  while(error > convergence & iter <= max.iter){
    # Perform iterations until error gets below threshold or max iter reached
    # Get kmeans centroids from each server (first iteration)
    cally <- paste0("kmeansDS(", x, ", ", paste0(centroids, collapse = ","), ")")
    
    new <- DSI::datashield.aggregate(datasources, as.symbol(cally))
    
    # Calculate weighted mean of the new centroids
    w <- matrix(0, ncol = k, nrow = length(columns))
    for(i in 1:length(columns)){
      for(j in 1:k){
        w[i,j] <- weighted.mean(unlist(lapply(new, function(x) x[[2]][j,i])), 
                                unlist(lapply(new, function(x) x[[1]][j,2])))
      }
    }
    error <- max(abs(w - centroids))
    centroids <- w
    iter <- iter + 1
  }
  
  # Assign the results as a new column if selected
  if(assign){
    if(is.null(name)){
      name <- x
      # Remove _aux from name to assign properly on the server side
      name <- gsub('.{4}$', '', x)
    }
    # Assign column with cluster grouping to the server dataset
    for(i in 1:length(new)){
      cally <- paste0("kmeans.assign_resultDS(",x, ",", paste0(new[[i]]$assignations, collapse = ","), ")")
      DSI::datashield.assign.expr(datasources[i], name, as.symbol(cally))
    }
  }
  
  # Return final centroids
  rownames(w) <- columns
  colnames(w) <- paste0("Centroid", 1:ncol(w))
  
  # Remove created auxiliary data frame
  ds.rm(x, datasources)
  return(w)
  
}