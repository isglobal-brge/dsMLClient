ds.build_tree <- function(object, objective_column, questions = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(!is.null(questions)){
    prepare_dataset_tree(object, questions, datasources)
  }
  else{
    ds.make(object, "tree_data", datasources)
  }
  
  best_split <- ds.find_best_split("tree_data", objective_column, datasources)
  
  if(best_split[1,1] == 0){browser()}
  
  questions <- questions_update(questions, best_split)
  
  ds.build_tree(object, objective_column, questions$normals)
  
  ds.build_tree(object, objective_column, questions$false)
  
  browser()
  
  
}
