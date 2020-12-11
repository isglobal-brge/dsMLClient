ds.find_best_split <- function(object, objective_column, datasources = NULL){
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  # Same cols assumed here
  cols <- ds.colnames(object, datasources = datasources)[[1]]
  cols <- cols[-which(cols == objective_column)]
  ginis <- NULL
  for(col in cols){
    ginis_aux <- ds.gini_index(object, col, objective_column, datasources)
    ginis <- rbind(ginis, ginis_aux)
  }
  
  return(ginis[which(ginis[,1] == min(ginis[,1]))[1],])
  
}
