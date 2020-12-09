#' @title Centering of a data frame on the study server
#' 
#' @description Centers each column of a data frame object by substracting the mean of the column
#'
#' @param x \code{character} Name of the data frame (or numeric vector, \code{table$column}) on the study
#'  server to be centered
#' @param type \code{type} (default \code{"combined"}) the centering is done with a pooled approach, using the
#' mean of the pooled data, if \code{"split"} the data will be centered with the mean of each server data
#' @param name \code{"character"} (default \code{NULL}) If \code{NULL}, the original data frame will be overwritten,
#' otherwise the centered data frame will be assigned to a variable named after this argument
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return This function does not have an output. It creates (or overwrites) a data frame on the study server.
#' @export

ds.center <- function(x, type = "combined", name = NULL, datasources = NULL){
  
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
      m <- ds.mean(x, type = "combine", datasources = datasources)$Global.Mean[1,1]
      
      cally <- paste0("centerDS(", x, ", ", paste0(m, collapse = ", "), ")")
      DSI::datashield.assign.expr(datasources, name, expr = as.symbol(cally))
      # Remove created auxiliary data frame
      ds.rm(x, datasources)
    }
    else{
      # Get variables of the table on the first study server (consistency of columns is supposed)
      vars <- ds.colnames(x, datasources)[[1]]
      
      # Create vector of names (table + $ + variables)
      table_columns <- as.list(paste0(x, "$", vars))
      
      # Get the mean of each variable (pooled mean)
      # this variable `m` holds a value for each column that will be subtracted from it on the server
      # to mean center the table, if someone wants to upgrade this function to perform a ¿median center?
      # change the following line so it returns a vector with the value to be subtracted by column
      m <- lapply(table_columns, function(v) ds.mean(v, type = "combine", datasources = datasources)$Global.Mean[1,1])
      
      cally <- paste0("centerDS(", x, ", ", paste0(m, collapse = ", "), ")")
      DSI::datashield.assign.expr(datasources, name, expr = as.symbol(cally))
      # Remove created auxiliary data frame
      ds.rm(x, datasources)
    }
  }
  else if(type == "split"){
    if(cls == "numeric"){
      m <- ds.mean(x, type = "split", datasources = datasources)$Mean.by.Study[, 1]
      
      for(srvr in 1:num_servers){
        cally <- paste0("centerDS(", x, ", ", m[srvr], ")")
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
      
      # Get the mean of each variable (split mean)
      m <- lapply(table_columns, function(v) ds.mean(v, type = "split", datasources = datasources)$Mean.by.Study[, 1])
      
      for(srvr in 1:num_servers){
        m_server <- lapply(m, `[[`, srvr)
        cally <- paste0("centerDS(", x, ", ", paste0(m_server, collapse = ", "), ")")
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