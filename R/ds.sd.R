#' @title Standard Deviation
#' 
#' @description This function computes the standard deviation of the values of a server numerical vector.
#' The SD can be calculated server-wise or pooled
#'
#' @param x \code{character} Name of the numeric vector on the server side to obtain it's SD
#' @param type \code{character} (default \code{"split"}) SD for each server (\code{"split"}) or
#' pooled approach (\code{"combined"})
#' @param checks \code{bool} (default \code{FALSE}) If \code{TRUE} optional checks of model components will be undertaken. 
#' It is suggested that checks should only be undertaken once the function call has failed.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{list} including:\cr
#' 
#'  \code{Standard_deviation.by.Study}: estimated standard deviation, \code{Nmissing}
#' (number of missing observations), \code{Nvalid} (number of valid observations) and
#' \code{Ntotal} (sum of missing and valid observations) 
#' separately for each study (if \code{type = split}).\cr
#' \code{Global.Standard_deviation}: estimated standard deviation, \code{Nmissing}, \code{Nvalid} and \code{Ntotal} 
#' across all studies combined (if \code{type = combine}). \cr
#' \code{Nstudies}: number of studies being analysed. \cr
#' \code{ValidityMessage}: indicates if the analysis was possible. \cr
#' @export

ds.sd <- function(x, type = 'split', checks = FALSE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  variance <- ds.var(x, type, checks, datasources = datasources)
    # SD = sqrt(var)
  if(type == "split"){
    variance$Variance.by.Study[, 1] <- sqrt(variance$Variance.by.Study[, 1])
    colnames(variance$Variance.by.Study) <- c("EstimatedSD", "Nmissing", "Nvalid", "Ntotal")
    names(variance) <- c("Standard_deviation.by.Study", "Nstudies", "ValidityMessage")
  }
  else if(type == "combined"){
    variance$Global.Variance[, 1] <- sqrt(variance$Global.Variance[, 1])
    colnames(variance$Global.Variance) <- c("EstimatedSD", "Nmissing", "Nvalid", "Ntotal")
    names(variance) <- c("Global.Standard_deviation", "Nstudies", "ValidityMessage")
  }
  else{stop("Invalid type argument, only 'split' and 'combined' allowed")}

  return(variance)
  
}
