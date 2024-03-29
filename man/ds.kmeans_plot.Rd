% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.kmeans_plot.R
\name{ds.kmeans_plot}
\alias{ds.kmeans_plot}
\title{Plot the results of a k-means analysis}
\usage{
ds.kmeans_plot(x, xcomp, ycomp, ellipses = TRUE, datasources = NULL)
}
\arguments{
\item{x}{\code{character} Name of the table on the server side with the results of the
k-means (\code{ds.kmeans(..., assign = TRUE)})}

\item{xcomp}{\code{character} Name of the variable on the 'x' data frame to be plotted on the x axis of the scatter plot}

\item{ycomp}{\code{character} Name of the variable on the 'y' data frame to be plotted on the y axis of the scatter plot}

\item{ellipses}{a flag indicates if ellipse is used}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
\code{ggplot} object
}
\description{
Wrapper of a scatter plot function that needs to be passed the results of \code{ds.kmeans} with
\code{assign = TRUE} to have the results on the server side.
}
