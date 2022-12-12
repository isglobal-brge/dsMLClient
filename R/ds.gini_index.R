#' Title
#'
#' @param object basis of gini index
#' @param column column needed for gini index
#' @param objective_column objective column needed for gini index
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return the gini index
#' @export
#'
ds.gini_index <- function(object, column, objective_column, datasources = NULL){

  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  gini_fun <- function(x){
    gini <- 1
    counts <- x[length(x)]
    for(i in 1:(length(x)-1)){
      gini <- gini - (x[i] / counts)^2
    }
    gini
  }
  
  # Get class of column 
  type <- dsBaseClient::ds.class(paste0(object, "$", column), datasources)
  
  if(all(type == "factor")){
    agg <- tryCatch({dsBaseClient::ds.table(paste0(object, "$", column), paste0(object, "$", objective_column), datasources = datasources)}, error = function(w){
      NULL
    })
    if(is.null(agg)){
      return(cbind(data.frame(gini_index = 1, column = column, 
                              question = NA, row.names = NULL)
                   , type = "factor"))}
    else if(!is.null(agg$error.messages)){
      return(cbind(data.frame(gini_index = 1, column = column, 
                              question = NA, row.names = NULL)
                   , type = "factor"))}
    # agg <- ds.table(paste0(object, "$", column), paste0(object, "$", objective_column), datasources = datasources)
    counts <- data.frame(agg$output.list$TABLES.COMBINED_all.sources_counts)
    counts <- cbind(counts, rowSums(counts))
    gini_index <- apply(counts, 1, gini_fun)
    gini_index <- cbind(data.frame(gini_index, column = column, question = paste("equal", names(gini_index), sep = "."),
                                   row.names = NULL), type = "factor")
  }
  else if(all(type == "numeric")){
    k <- dsBaseClient::ds.mean(paste0(object, "$", column), type = "combine", datasources = datasources)$Global.Mean[1]
      # Create a vector with all k
    dsBaseClient::ds.make(toAssign = paste0(object, "$", column, "-", object, "$", column, "+", k),
                          newobj = "ds.gini_index_ks",
                          datasources = datasources) 
    dsBaseClient::ds.dataFrameSubset(df.name = object, V1.name = paste0(object, "$", column), 
                                     V2.name = "ds.gini_index_ks", Boolean.operator =  ">", 
                                     newobj = "ds.gini_index_upper", keep.NAs = T, datasources = datasources)
    dsBaseClient::ds.dataFrameSubset(df.name = object, V1.name = paste0(object, "$", column), 
                                     V2.name = "ds.gini_index_ks", Boolean.operator =  "<=", 
                                     newobj = "ds.gini_index_lower", keep.NAs = T, datasources = datasources)
    # PETE AQUI, PERQUE PETE!!!!!!!!!?????????
    agg_low <- tryCatch({dsBaseClient::ds.table(paste0("ds.gini_index_lower$", objective_column), datasources = datasources)}, 
             error = function(w){
               DSI::datashield.rm(datasources, "ds.gini_index_ks")
               DSI::datashield.rm(datasources, "ds.gini_index_upper")
               DSI::datashield.rm(datasources, "ds.gini_index_lower")
               NULL
               })
    if(is.null(agg_low) | !is.null(agg_low$error.messages)){
      return(cbind(data.frame(gini_index = 1, column = column, 
                                                 question = NA, row.names = NULL)
                                      , type = "numerical"))}
    agg_upp <- tryCatch({dsBaseClient::ds.table(paste0("ds.gini_index_upper$", objective_column), datasources = datasources)}, 
                        error = function(w){
                          DSI::datashield.rm(datasources, "ds.gini_index_ks")
                          DSI::datashield.rm(datasources, "ds.gini_index_upper")
                          DSI::datashield.rm(datasources, "ds.gini_index_lower")
                          NULL
                        })
    if(is.null(agg_upp) | !is.null(agg_low$error.messages)){
      return(cbind(data.frame(gini_index = 1, column = column, 
                              question = NA, row.names = NULL)
                   , type = "numerical"))}
    counts <- data.frame(rbind(lower = agg_low$output.list$TABLES.COMBINED_all.sources_counts,
                    upper = agg_upp$output.list$TABLES.COMBINED_all.sources_counts))
    colnames(counts) <- rownames(data.frame(agg_low$output.list$TABLES.COMBINED_all.sources_counts))
    counts <- cbind(counts, rowSums(counts))
    gini_index <- apply(counts, 1, gini_fun)
    gini_index <- cbind(data.frame(gini_index, column = column, 
                                             question = paste(names(gini_index), k, sep = "."), row.names = NULL)
                                  , type = "numerical")
  }
  
  DSI::datashield.rm(datasources, "ds.gini_index_ks")
  DSI::datashield.rm(datasources, "ds.gini_index_upper")
  DSI::datashield.rm(datasources, "ds.gini_index_lower")
  
  return(gini_index)
  
}
