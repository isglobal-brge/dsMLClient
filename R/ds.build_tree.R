#' Title
#'
#' @param object 
#' @param objective_column 
#' @param questions 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.build_tree <- function(object, objective_column, questions = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  if(!is.null(questions)){
    data_status <- prepare_dataset_tree(object, objective_column, questions, datasources)
    if(data_status == "LEAF"){
      # print(leaf(questions, objective_column, datasources))
      return(leaf(object, questions, objective_column, datasources))
    }
  }
  else{
    ds.make(object, "tree_data", datasources)
  }
  
  best_split <- ds.find_best_split("tree_data", objective_column, datasources)
  
  if(best_split[1,1] == 0 | best_split[1,1] == 1){
    return(leaf(object, questions, objective_column, datasources))
  }
  
  questions <- questions_update(questions, best_split)
  # print(questions$normals)
  ds.build_tree(object, objective_column, questions$normals, datasources)
  # print(questions$false)
  ds.build_tree(object, objective_column, questions$false, datasources)
  
  # print("una branca acabada lesgo")
  
  
}
