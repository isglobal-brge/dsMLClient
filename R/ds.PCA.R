#' @title Principal component analysis (PCA)
#' 
#' @description Perform a non-disclosive PCA with the option of centering and scaling (unit variance) the pooled data
#'
#' @param x \code{character} Name of the data frame on the server that has the dataset
#' @param method \code{character} (default \code{"fast"}) Method of the SVD calculation. \code{"fast"} to do a truncated SVD of the first \code{ncomp}
#' components, \code{"full"} to do the complete SVD calculation.
#' @param ncomp \code{numeric} (default \code{2}) Number of components to calculate when \code{method = "fast"}
#' @param scale \code{bool} (default \code{TRUE}) Set to TRUE to scale the dataset (pooled approach), FALSE to not scale
#' @param center \code{bool} (default \code{TRUE}) Set to TRUE to center the dataset (pooled approach), FALSE to not scale
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{data frame} With the principal components (columns) and variables (rows)
#' @export

ds.PCA <- function(x, method = "fast", ncomp = 2, scale = TRUE, center = TRUE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  
  if(!any(cls %in% c("data.frame"))){
    stop("The input vector is not of class 'data.frame'")
  }
  
  # Create a copy of the data frame (with the numerical columns only!)
  ds.subset_type(x, type = "numeric", newobj = paste0(x, "_aux"), datasources)
  
  x <- paste0(x, "_aux")
  
  if(center){
    ds.center(x, "combined", NULL, datasources)
  }
  
  if(scale){
    ds.scale(x, "combined", NULL, datasources)
  }
  
  var_cov <- ds.cov(x, type = "combine", datasources = datasources)$`Variance-Covariance Matrix`
  
  if(all(is.na(var_cov))){
    stop("The ratio of the number of variables over the number of individual-level records exceeds 
         the allowed threshold, there is a possible risk of disclosure")
  }
  
  if(method == "fast"){
    var_cov_svd <- irlba::irlba(var_cov, ncomp)
  }
  else if(method == "full"){
    var_cov_svd <- svd(var_cov)
  }
  else(stop(paste0("Invalid method: ", method, ". Supported methods are 'fast'/'full'")))
  
  pca <- data.frame(var_cov_svd$u)
  
  rownames_pca <- ds.colnames(x)
  rownames(pca) <- rownames_pca[[1]]
  colnames(pca) <- paste0(rep("PC", ncol(pca)),
                          seq(from = 1, by = 1, length.out = ncol(pca)))
  
  # Remove created auxiliary data frame
  ds.rm(x, datasources)
  
  return(pca)
  
}