#' @title Singular value decomposition (SVD)
#' 
#' @description Non-disclosive SVD on pooled data
#'
#' @param x \code{character} Name of the data frame on the server that has the dataset
#' @param method.block \code{character} (default \code{"fast"}) Method of the SVD calculation. \code{"fast"} 
#' to do a truncated SVD of the first \code{ncomp} components, \code{"full"} to do the complete SVD calculation
#' @param ncomp \code{numeric} (default \code{2}) Number of components to calculate when \code{method = "fast"}
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{list} with: \cr
#' -d \code{numeric}: singular values of x, sorted decreasingly\cr
#' -u \code{numeric data frame}:left singular vectors\cr
#' @export

ds.svd <- function(x, method.block = "fast", ncomp = 2, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  # scale i center
  
  # Create a copy of the data frame (with the numerical columns only!)
  ds.subset_type(x, type = "numeric", newobj = paste0(x, "_aux"), datasources)
  
  x <- paste0(x, "_aux")
  
  cally <- paste0("svdDS(", x, ")")
  xx <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  X <- Reduce(cbind, xx)
  
  if(method.block == "full"){
    s <- svd(X)
    ans <- list(d = s$d, v = s$u)
  }
  
  if(method.block == "fast"){
    s <- irlba::irlba(X, nv = 0, nu = ncomp)
    ans <- list(d = s$d, v = s$u)
  }
  
  # Remove created auxiliary data frame
  dsBaseClient::ds.rm(x, datasources)
  return(ans)
  
}
