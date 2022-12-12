#' Title
#'
#' @param questions basis of questions update
#' @param best_split best split needed for questions update
#'
#' @return the question update
#' @export
#'
questions_update <- function(questions, best_split){
  
  if(best_split[,4] == "numerical"){
    if(substr(best_split[1,3], 1, 5) == "lower"){
      inverse_best_split <- cbind(best_split[,1:2], 
                                  question = paste0("upper", substr(best_split[1,3], 6, nchar(best_split[1,3]))),
                                  type = best_split[,4])
    }
    else{
      inverse_best_split <- cbind(best_split[,1:2], 
                                  question = paste0("lower", substr(best_split[1,3], 6, nchar(best_split[1,3]))),
                                  type = best_split[,4])
    }
  }
  else{
    if(substr(best_split[1,3], 1, 5) == "equal"){
      inverse_best_split <- cbind(best_split[,1:2], 
                                  question = paste0("nqual", substr(best_split[1,3], 6, nchar(best_split[1,3]))),
                                  type = best_split[,4])
    }
    else{
      inverse_best_split <- cbind(best_split[,1:2], 
                                  question = paste0("equal", substr(best_split[1,3], 6, nchar(best_split[1,3]))),
                                  type = best_split[,4])
    }
  }
  
  return(list(normals = rbind(questions, best_split), false = rbind(questions, inverse_best_split)))
  
}
