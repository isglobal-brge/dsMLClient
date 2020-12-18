#' Title
#'
#' @param object 
#' @param questions 
#' @param objective_column 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
leaf <- function(object, questions, objective_column, datasources){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  perc <- tryCatch({
    ds.table(paste0("tree_data$", objective_column), datasources = datasources)$output.list$TABLES.COMBINED_all.sources_proportions
  }, error = function(w){
    prepare_dataset_tree(object, objective_column, questions[1:nrow(questions)-1,], datasources)
    ds.table(paste0("tree_data$", objective_column), datasources = datasources)$output.list$TABLES.COMBINED_all.sources_proportions
  })

  return(list(questions, perc))
  
}
