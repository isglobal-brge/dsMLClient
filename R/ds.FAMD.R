#' @title Factor Analysis on Mixed Data
#' 
#' @description Perform FAMD analysis on server data
#'
#' @param x \code{character} Name of the data frame on the study server
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{data frame} With the principal components (columns) and variables (rows)
#' @export

ds.FAMD <- function(x, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any(cls %in% c("data.frame"))){
    stop("The selected object [", x, "] is not a 'data.frame'")
  }
  
  ds.subset_type(x, type = "numeric", newobj = paste0(x, "_numerics"), datasources)
  ds.subset_type(x, type = "factor", newobj = paste0(x, "_factors"), datasources)
  
  # ds.center(paste0(x, "_numerics"), "combined", NULL, datasources)
  ds.scale(paste0(x, "_numerics"), "combined", NULL, datasources)
  
  ds.dummies(paste0(x, "_factors"), NULL, TRUE, datasources)
  
  ds.cbind(
    x = c(paste0(x, "_numerics"),
          paste0(x, "_factors")), 
    newobj = paste0(x, "_complete"), 
    datasources = datasources
  )
  # cally <- paste0("cbind(", paste0(x, "_numerics"), ", ", paste0(x, "_factors"), ")")
  # DSI::datashield.assign.expr(datasources, paste0(x, "_complete"), as.symbol(cally))
  
  ds.PCA(paste0(x, "_complete"), method = "full", scale = FALSE, center = FALSE, datasources = datasources)
  
}