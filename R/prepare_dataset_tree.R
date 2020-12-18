#' Title
#'
#' @param object 
#' @param questions 
#' @param datasources 
#'
#' @return
#' @export
#'
#' @examples
prepare_dataset_tree <- function(object, objective_column, questions, datasources){
  
  cally <- paste0("prepare_dataset_treeDS(", object, ", '", sf::rawToHex(serialize(questions, NULL)), "')")
  
  tryCatch({
    DSI::datashield.assign.expr(datasources, "tree_data", as.symbol(cally))
    return("OK")
  }, error = function(w){
    cally <- paste0("prepare_dataset_treeDS(", object, ", '", sf::rawToHex(serialize(questions[1:nrow(questions)-1,], NULL)), "')")
    DSI::datashield.assign.expr(datasources, "tree_data", as.symbol(cally))
    return("LEAF")
  })
  
  # Si aixo pete vol dir que estem al cas negatiu, ia que el positiu ia passe un control al
  # calcular el gini score (si no el passe es descarte). Per tant si aixo pete trec la ultima
  # fila de questions i crido un altre cop el prepare_dataset_tree (recursio ftw) 
  # i just despres la funció leaf() amb totes les questions, he de fer dalguna forma que 
  # li puga dir que els percentatges que treura la funcio leaf shan de calcular amb una row 
  # menys de questions, tot i que les vull incloure totes a lhora de fer predicció
  
}
