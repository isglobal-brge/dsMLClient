#' Title
#'
#' @param object basis of leaf
#' @param questions questions needed for leaf.
#' @param objective_column objective column needed for leaf.
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return the leaf
#' @export
#'
leaf <- function(object, questions, objective_column, datasources){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  perc <- tryCatch({
    dsBaseClient::ds.table(paste0("tree_data$", objective_column), datasources = datasources)$output.list$TABLES.COMBINED_all.sources_proportions
  }, error = function(w){
    prepare_dataset_tree(object, objective_column, questions[1:nrow(questions)-1,], datasources)
    dsBaseClient::ds.table(paste0("tree_data$", objective_column), datasources = datasources)$output.list$TABLES.COMBINED_all.sources_proportions
  })

  return(list(questions, perc))
  
}
