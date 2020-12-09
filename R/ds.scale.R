#' @title Scaling of a server side data frame or numeric vector
#' 
#' @description Scales by dividing each variable by the standard deviation to have unitary variance
#'
#' @param x \code{data.frame} or \code{numeric} vector to be scaled
#' @param type \code{type} (default \code{"combined"}) the scaling is done with a pooled approach, using the
#' standard deviation of the pooled data, if \code{"split"} the data will be scaled with the standard deviation of each server data
#' @param name \code{"character"} (default \code{NULL}) If \code{NULL}, the original data frame will be overwritten,
#' otherwise the scaled data frame will be assigned to a variable named after this argument
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates (or overwrites) a data frame on the study server.
#' @export

ds.scale <- function(x, type = "combined", name = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(is.null(name)){
    name <- x
  }
  
  dsBaseClient:::isDefined(datasources, x)
  cls <- dsBaseClient:::checkClass(datasources, x)
  if(!any(cls %in% c("data.frame", "numeric"))){
    stop("The input vector is not of class 'numeric' or it's not a 'data.frame'")
  }
  
  if(any(cls %in% c("data.frame"))){
    # Create a copy of the data frame (with the numerical columns only!)
    ds.subset_type(x, type = "numeric", newobj = paste0(x, "_aux"), datasources)
    x <- paste0(x, "_aux")
  }
  else{
    cally <- x
    DSI::datashield.assign.expr(datasources, "aux_num_vector", as.symbol(cally))
    x <- "aux_num_vector"
  }
  
  # Get number of study servers
  num_servers <- length(names(datasources))
  
  if(type == "combined"){
    if(cls == "numeric"){
      sd <- ds.sd(x, type = "combined", datasources = datasources)$Global.Standard_deviation[1,1]
      
      cally <- paste0("scaleDS(", x, ", ", paste0(sd, collapse = ", "), ")")
      DSI::datashield.assign.expr(datasources, name, expr = as.symbol(cally))
      # Remove created auxiliary data frame
      ds.rm(x, datasources)
    }
    else{
      # Get variables of the table on the first study server (consistency of columns is supposed)
      vars <- ds.colnames(x, datasources)[[1]]
      
      # Create vector of names (table + $ + variables)
      table_columns <- as.list(paste0(x, "$", vars))
      
      # Get the sd of each variable (pooled sd)
      # this variable `sd` holds a value for each column to divide the column and get unitary variance
      sd <- lapply(table_columns, function(v) ds.sd(v, type = "combined", datasources = datasources)$Global.Standard_deviation[1,1])
      
      cally <- paste0("scaleDS(", x, ", ", paste0(sd, collapse = ", "), ")")
      DSI::datashield.assign.expr(datasources, name, expr = as.symbol(cally))
      # Remove created auxiliary data frame
      ds.rm(x, datasources)
    }
  }
  else if(type == "split"){
    if(cls == "numeric"){
      sd <- ds.sd(x, type = "split", datasources = datasources)$Standard_deviation.by.Study[, 1]
      
      for(srvr in 1:num_servers){
        cally <- paste0("scaleDS(", x, ", ", sd[srvr], ")")
        DSI::datashield.assign.expr(datasources[srvr], name, expr = as.symbol(cally))
        # Remove created auxiliary data frame
        ds.rm(x, datasources)
      }
    }
    else{
      # Get variables of the table on the first study server (consistency of columns is supposed)
      vars <- ds.colnames(x, datasources)[[1]]
      
      # Create vector of names (table + $ + variables)
      table_columns <- as.list(paste0(x, "$", vars))
      
      # Get the sd of each variable (split sd)
      sd <- lapply(table_columns, function(v) ds.sd(v, type = "split", datasources = datasources)$Standard_deviation.by.Study[, 1])
      
      for(srvr in 1:num_servers){
        sd_server <- lapply(sd, `[[`, srvr)
        cally <- paste0("scaleDS(", x, ", ", paste0(sd_server, collapse = ", "), ")")
        DSI::datashield.assign.expr(datasources[srvr], name, expr = as.symbol(cally))
        # Remove created auxiliary data frame
        ds.rm(x, datasources)
      }
    }
  }
  else{
    # Remove created auxiliary data frame
    ds.rm(x, datasources)
    stop(paste0("Not valid 'type' argument:", type))
    }
}
