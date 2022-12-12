#' @title Plot the results of a k-means analysis
#' 
#' @description Wrapper of a scatter plot function that needs to be passed the results of \code{ds.kmeans} with 
#' \code{assign = TRUE} to have the results on the server side.
#'
#' @param x \code{character} Name of the table on the server side with the results of the 
#' k-means (\code{ds.kmeans(..., assign = TRUE)})
#' @param xcomp \code{character} Name of the variable on the 'x' data frame to be plotted on the x axis of the scatter plot
#' @param ycomp \code{character} Name of the variable on the 'y' data frame to be plotted on the y axis of the scatter plot
#' @param ellipses a flag indicates if ellipse is used
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @return \code{ggplot} object
#' @export

ds.kmeans_plot <- function(x, xcomp, ycomp, ellipses = TRUE, datasources = NULL){
  
  if (is.null(datasources)) {
    datasources <- DSI::datashield.connections_find()
  }
  
  plt <- ds.scatterPlotGG(x = paste0(x, "$", xcomp), y = paste0(x, "$", ycomp), 
                          group = paste0(x, "$kmeans.cluster"), type = "combine", datasources = datasources)
  if(ellipses){
    plt <- plt + ggplot2::stat_ellipse(type = "norm", linetype = 2)
  }
  
  plt$labels$colour <- "Cluster"
  
  return(plt)
  
}
