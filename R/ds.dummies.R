#' @title Create dummy variables
#' 
#' @description Create dummy variables for factor columns on server data frame
#'
#' @param x \code{character} Name of the data frame on the study server
#' @param name \code{character} (default \code{NULL}) Overwrites the data frame if \code{NULL},
#' otherwise the new data frame with dummies is asigned to a new variable
#' @param prob_adjust \code{bool} (default \code{FALSE}) Scale the dummy variables dividing them
#' by the square root of their probability (n/n_total)
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates (or overwrites) a data frame on the study server.
#' @export

ds.dummies <- function(x, name = NULL, prob_adjust = FALSE, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- x
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any(cls %in% c("data.frame"))){
    stop("The selected object [", x, "] is not a 'data.frame'")
  }
  
  cally <- paste0("dummiesDS(", x, ")")
  DSI::datashield.assign.expr(datasources, name, as.symbol(cally))
  
  if(prob_adjust == TRUE){
    ds.dummy_probability(name, datasources)
  }
  
}