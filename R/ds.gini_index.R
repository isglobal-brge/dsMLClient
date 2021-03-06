#' Title
#'
#' @param object 
#' @param column 
#' @param objective_column 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.gini_index <- function(object, column, objective_column, datasources = NULL){

  if(is.null(datasources)){
    datasources <- datashield.connections_find()
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
  type <- ds.class(paste0(object, "$", column), datasources)
  
  if(all(type == "factor")){
    agg <- tryCatch({ds.table(paste0(object, "$", column), paste0(object, "$", objective_column), datasources = datasources)}, error = function(w){
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
    k <- ds.mean(paste0(object, "$", column), type = "combine", datasources = datasources)$Global.Mean[1]
      # Create a vector with all k
    ds.make(toAssign = paste0(object, "$", column, "-", object, "$", column, "+", k),
             newobj = "ds.gini_index_ks",
             datasources = datasources) 
    ds.dataFrameSubset(df.name = object, V1.name = paste0(object, "$", column), 
                       V2.name = "ds.gini_index_ks", Boolean.operator =  ">", 
                       newobj = "ds.gini_index_upper", keep.NAs = T, datasources = datasources)
    ds.dataFrameSubset(df.name = object, V1.name = paste0(object, "$", column), 
                       V2.name = "ds.gini_index_ks", Boolean.operator =  "<=", 
                       newobj = "ds.gini_index_lower", keep.NAs = T, datasources = datasources)
    # PETE AQUI, PERQUE PETE!!!!!!!!!?????????
    agg_low <- tryCatch({ds.table(paste0("ds.gini_index_lower$", objective_column), datasources = datasources)}, 
             error = function(w){
               datashield.rm(datasources, "ds.gini_index_ks")
               datashield.rm(datasources, "ds.gini_index_upper")
               datashield.rm(datasources, "ds.gini_index_lower")
               NULL
               })
    if(is.null(agg_low) | !is.null(agg_low$error.messages)){
      return(cbind(data.frame(gini_index = 1, column = column, 
                                                 question = NA, row.names = NULL)
                                      , type = "numerical"))}
    agg_upp <- tryCatch({ds.table(paste0("ds.gini_index_upper$", objective_column), datasources = datasources)}, 
                        error = function(w){
                          datashield.rm(datasources, "ds.gini_index_ks")
                          datashield.rm(datasources, "ds.gini_index_upper")
                          datashield.rm(datasources, "ds.gini_index_lower")
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
  
  datashield.rm(datasources, "ds.gini_index_ks")
  datashield.rm(datasources, "ds.gini_index_upper")
  datashield.rm(datasources, "ds.gini_index_lower")
  
  return(gini_index)
  
}
