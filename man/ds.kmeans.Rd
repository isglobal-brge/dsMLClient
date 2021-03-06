% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ds.kmeans.R
\name{ds.kmeans}
\alias{ds.kmeans}
\title{K-Means clustering of distributed table}
\usage{
ds.kmeans(
  x,
  k = NULL,
  convergence = 0.001,
  max.iter = 100,
  centroids = NULL,
  assign = TRUE,
  name = NULL,
  datasources = NULL
)
}
\arguments{
\item{x}{\code{character} Name of the data frame on the study server with the data to train the k-means}

\item{k}{\code{numeric} Integer numeric with the number of clusters to find}

\item{convergence}{\code{numeric} (default \code{0.001}) Threshold error for the iterations}

\item{max.iter}{\code{numeric} (default \code{100}) Maxim number of iterations to stop the algorithm}

\item{centroids}{\code{data frame} (default \code{NULL}) If \code{NULL} random starting centroids will be calculated
using the 10/90 inter-quartile range. If a value is supplied, those centroids will be used to start the algorithm.
Structure of the data frame to be supplied: \cr
\itemize{
\item Each column corresponds to a centroid, so 3 columns correspond to a k-means with k = 3 \cr
\item Each row corresponds to the value of each variable, this has to match the data frame of name 'x' on the server
in both length and order.
}}

\item{assign}{\code{bool} (default \code{TRUE}) If \code{TRUE} the results of the cluster will be added to the data frame
on the server side}

\item{name}{\code{character} (default \code{NULL}) If \code{NULL} and \code{assign = TRUE}, the original table 'x'
will be overwritten on the server side with an additional column named 'kmeans.cluster' that contain the results
of the k-means. If a value is provided on this argument, a new object on the server side will be created with the
values from the original table 'x' + the new 'kmeans.cluster' column.}

\item{datasources}{a list of \code{\link{DSConnection-class}} (default \code{NULL}) objects obtained after login}
}
\value{
\code{data frame} Where: \cr
-Each column corresponds to a centroid (1:k) \cr
-Each row corresponds to the a variable of the server data frame
}
\description{
Performs a k-means clustering on a distributed table using euclidean distance
}
\details{
This implementation of the kmeans is basically a parallel kmeans where each server acts as a thread. It can be
applied because the results that are passed to the master (client) are not disclosive since they are aggregated values
that cannot be traced backwards. The assignations vector is not disclosive since all the information that can be
extracted from it is the same given by the ds.summary function. For more information on the implementation please refer
to 'Parallel K-Means Clustering Algorithm on DNA Dataset' by Fazilah Othman, RosniAbdullah, Nur’Aini Abdul Rashid and
Rosalina Abdul Salam
}
