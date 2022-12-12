#' @title Scale dummy variables
#' 
#' @description Internal function. Scale using the square root of their probabilitiy.
#'
#' @param x \code{character} Name of the data frame on the study server
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates (or overwrites) a data frame on the study server.

ds.dummy_probability <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any(cls %in% c("dummies"))){
    stop("The selected object [", x, "] is not a 'dummies', use ds.dummies() to create one")
  }
  
  cally <- paste0("dummy_probabilityDS(", x, ")")
  counts <- DSI::datashield.aggregate(datasources, as.symbol(cally))
  totals <- dsBaseClient::ds.dim(x)[[3]][1]
  
  probs <- matrix(unlist(counts), nrow=length(counts), byrow=T)
  probs <- probs / totals
  probs <- colSums(probs)

  cally <- paste0("dummies_transformationDS(", x, ", ", paste0(probs, collapse = ", "), ")")
  DSI::datashield.assign.expr(datasources, x, as.symbol(cally))
  
}
