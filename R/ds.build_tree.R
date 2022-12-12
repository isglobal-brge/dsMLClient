#' Title
#'
#' @param object basis of build tree
#' @param objective_column objective columns needed to build tree
#' @param questions questions needed to build tree
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified
#' the default set of connections will be used: see \code{\link{datashield.connections_default}}.
#'
#' @return the built tree
#' @export
#'
ds.build_tree <- function(object, objective_column, questions = NULL, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }
  
  if(!is.null(questions)){
    data_status <- prepare_dataset_tree(object, objective_column, questions, datasources)
    if(data_status == "LEAF"){
      # print(leaf(questions, objective_column, datasources))
      return(leaf(object, questions, objective_column, datasources))
    }
  }
  else{
    dsBaseClinet::ds.make(object, "tree_data", datasources)
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
