prepare_dataset_tree <- function(object, questions, datasources){
  
  cally <- paste0("prepare_dataset_treeDS(", object, ", ", serialize(questions, NULL), ")")
  DSI::datashield.assign.expr(datasources, "tree_data", as.symbol(cally))
  
}
