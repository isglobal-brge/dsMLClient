#' @title Generates non-disclosive scatter plots (with \code{ggplot2})
#' 
#' @description This function uses two disclosure control methods to generate non-disclosive scatter
#' plots of two server-side continuous variables, with the option of using a grouping factor variable.
#'
#' @param x \code{character} (default \code{NULL}) specifying the name of a numeric vector.
#' @param y \code{character} (default \code{NULL}) specifying the name of a numeric vector.
#' @param group \code{character} (default \code{NULL}) specifying the name of a numeric vector.
#' @param method \code{character} (default \code{'deterministic'}) string that specifies the method that is used to
#' generated non-disclosive coordinates to be displayed in a scatter plot. This argument can be set as \code{'deteministic'}
#'  or \code{'probabilistic'}. For more information see Details.
#' @param k \code{numeric} (default \code{3}) he number of the nearest neighbors for which their centroid is calculated. 
#' For more information see Details.
#' @param noise \code{numeric} (default \code{0.25}) the percentage of the initial variance that is used as the variance 
#' of the embedded noise if the argument method is set to \code{'probabilistic'}. 
#' @param type \code{character} (default \code{'split'}) a character that represents the type of graph to display. 
#' This can be set as \code{'combine'} or \code{'split'}. For more information see Details.
#' @param datasources a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login
#'
#' @details As the generation of a scatter plot from original data is disclosive and is not
#' permitted in DataSHIELD, this function allows the user to plot non-disclosive scatter plots.
#' 
#' If the argument \code{method} is set to \code{'deterministic'}, the server-side function searches
#' for the \code{k-1} nearest neighbors of each single data point and calculates the centroid 
#' of such \code{k} points. 
#' The proximity is defined by the minimum Euclidean distances of z-score transformed data.
#' 
#' When the coordinates of all centroids are estimated the function applies scaling to expand the
#' centroids back to the dispersion of the original data. The scaling is achieved by multiplying
#' the centroids with a scaling factor that is equal to the ratio between the standard deviation of
#' the original variable and the standard deviation of the calculated centroids. The coordinates of
#' the scaled centroids are then returned to the client-side.
#' 
#' The value of \code{k} is specified by the user. 
#' The suggested and default value is equal to 3 which is also
#' the suggested minimum threshold that is used to prevent disclosure which is specified in the
#' protection filter \code{nfilter.kNN}. When the value of \code{k} increases, 
#' the disclosure risk decreases but the utility loss increases.
#' The value of \code{k} is used only
#' if the argument \code{method} is set to \code{'deterministic'}. 
#' Any value of \code{k} is ignored if the
#' argument \code{method} is set to \code{'probabilistic'}.
#' 
#' If the argument \code{method} is set to \code{'probabilistic'}, 
#' the server-side function generates a random normal noise of zero mean
#' and variance equal to 10\% of the variance of each \code{x} and \code{y} variable.
#' The noise is added to each \code{x} and \code{y} variable and the disturbed by the addition of
#' \code{noise} data are returned to the client-side. Note that the seed random number generator is fixed to a
#' specific number generated from the data and therefore the user gets the same figure every time
#' that chooses the probabilistic method in a given set of variables.
#' The value of \code{noise} is used only if the argument \code{method} is set to \code{'probabilistic'}.
#' Any value of \code{noise} is ignored if
#' the argument \code{method} is set to \code{'deterministic'}. 
#' 
#' In \code{type} argument can be set two graphics to display:\cr
#' (1) If \code{type = 'combine'}  a scatter plot for
#' combined data is generated.\cr
#' (2) If \code{type = 'split'}  one scatter plot for each
#' study is generated. 
#' 
#' Server function called: \code{scatterPlotGGDS}
#' 
#' @return \code{ggplot} object
#' @export

ds.scatterPlotGG <- function (x=NULL, y=NULL, group = NULL, method='deterministic', k=3, noise=0.25, type="split", datasources=NULL){
  
  # look for DS connections
  if(is.null(x)){
    stop("Please provide the x-variable", call.=FALSE)
  }
  
  if(is.null(y)){
    stop("Please provide the y-variable", call.=FALSE)
  }
  
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }
  
  # the input variable might be given as column table (i.e. D$object)
  # or just as a vector not attached to a table (i.e. object)
  # we have to make sure the function deals with each case
  objects <- c(x, y)
  xnames <- extract(objects)
  varnames <- xnames$elements
  obj2lookfor <- xnames$holders
  
  # check if the input object(s) is(are) defined in all the studies
  for(i in 1:length(varnames)){
    if(is.na(obj2lookfor[i])){
      defined <- isDefined(datasources, varnames[i])
    }else{
      defined <- isDefined(datasources, obj2lookfor[i])
    }
  }
  
  # call the internal function that checks the input object(s) is(are) of the same class in all studies.
  typ.x <- checkClass(datasources, x)
  typ.y <- checkClass(datasources, y)
  
  # the input objects must be numeric or integer vectors
  if(!('integer' %in% typ.x) & !('numeric' %in% typ.x)){
    message(paste0(x, " is of type ", typ.x, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  if(!('integer' %in% typ.y) & !('numeric' %in% typ.y)){
    message(paste0(y, " is of type ", typ.y, "!"))
    stop("The input objects must be integer or numeric vectors.", call.=FALSE)
  }
  
  # the input variable might be given as column table (i.e. D$x)
  # or just as a vector not attached to a table (i.e. x)
  # we have to make sure the function deals with each case
  xnames <- extract(x)
  x.lab <- xnames[[length(xnames)]]
  ynames <- extract(y)
  y.lab <- ynames[[length(ynames)]]
  
  # name of the studies to be used in the plots' titles
  stdnames <- names(datasources)
  
  # number of studies
  num.sources <- length(datasources)
  
  if(method=='deterministic'){ method.indicator <- 1 }
  if(method=='probabilistic'){ method.indicator <- 2 }
  
  # call the server-side function that generates the x and y coordinates of the centroids
  call <- paste0("scatterPlotGGDS(", x, ",", y, ",", group, ",", method.indicator, ",", k, ",", noise, ")")
  output <- DSI::datashield.aggregate(datasources, call)
  
  pooled.points.x <- c()
  pooled.points.y <- c()
  pooled.group <- c()
  for (i in 1:num.sources){
    pooled.points.x[[i]] <- output[[i]][[1]]
    pooled.points.y[[i]] <- output[[i]][[2]]
    if(!is.null(group)){pooled.group[[i]] <- output[[i]][[3]]}
  }
  pooled.points.x <- unlist(pooled.points.x)
  pooled.points.y <- unlist(pooled.points.y)
  pooled.group <- unlist(pooled.group)
  
  datapoints <- data.table::data.table(pooled.points.x, pooled.points.y, pooled.group)
  
  # plot and return the scatter plot depending on the argument "type"
  if(type=="combine"){
    numr <- 1
    numc <- 1
    
    if(is.null(group)){
      plt <- ggplot2::ggplot(datapoints, ggplot2::aes(x = pooled.points.x, y = pooled.points.y)) +
        ggplot2::geom_point() +
        ggplot2::xlab(x.lab) +
        ggplot2::ylab(y.lab) +
        ggplot2::ggtitle(paste0("Combined scatter plot"))
    }
    else{
      plt <- ggplot2::ggplot(datapoints, ggplot2::aes(x = pooled.points.x, y = pooled.points.y, color = pooled.group)) +
        ggplot2::geom_point() +
        ggplot2::xlab(x.lab) +
        ggplot2::ylab(y.lab) +
        ggplot2::ggtitle(paste0("Combined scatter plot"))
    }
    
    return(plt)
    
  }else{
    if(type=="split"){
      # set the graph area and plot
      if(num.sources > 1){
        if((num.sources %% 2) == 0){ numr <- num.sources/2 }else{ numr <- (num.sources+1)/2}
        numc <- 2
        # graphics::par(mfrow=c(numr,numc))
        scatter <- list()
      }
      for(i in 1:num.sources){
        title <- paste0("Scatter plot of ", stdnames[i])
        x <- output[[i]][[1]]
        y <- output[[i]][[2]]
        group <- output[[i]][[3]]
        datapoints <- data.table::data.table(x, y, group)
        if(is.null(group)){
          scatter[[i]] <- ggplot2::ggplot(datapoints, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point() +
            ggplot2::xlab(x.lab) +
            ggplot2::ylab(y.lab) +
            ggplot2::ggtitle(title)
        }
        else{
          scatter[[i]] <- ggplot2::ggplot(datapoints, ggplot2::aes(x = x, y = y, color = group)) +
            ggplot2::geom_point() +
            ggplot2::xlab(x.lab) +
            ggplot2::ylab(y.lab) +
            ggplot2::ggtitle(title)
        }
      }
      
      plt <- gridExtra::grid.arrange(grobs = scatter, ncol = numc)

      return(plt)
      
    }else{
      stop('Function argument "type" has to be either "combine" or "split"')
    }
  }
}
#ds.scatterPlotGG