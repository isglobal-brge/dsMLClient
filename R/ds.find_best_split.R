#' Title
#'
#' @param object 
#' @param objective_column 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
ds.find_best_split <- function(object, objective_column, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  # Same cols assumed here
  cols <- ds.colnames(object, datasources = datasources)[[1]]
  cols <- cols[-which(cols == objective_column)]
  # if(!is.null(questions)){
  #   cols <- cols[-which(cols %in% questions[,2])]
  # }
  ginis <- NULL
  for(col in cols){
    ginis_aux <- ds.gini_index(object, col, objective_column, datasources)
    ginis <- rbind(ginis, ginis_aux)
  }

  ginis <- ginis[!is.nan(ginis[,1]),]

  return(ginis[which(ginis[,1] == min(ginis[,1]))[1],])
  
}
