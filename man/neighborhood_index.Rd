% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neighborhood_index.R
\name{neighborhood_index}
\alias{neighborhood_index}
\title{Get Neighborhood Matrix of Indices from an Image}
\usage{
neighborhood_index(img, ...)
}
\arguments{
\item{img}{Object of class \code{\link{nifti}}, character or \code{antsImage}}

\item{...}{arguments other than \code{get.gradient} passed to 
\code{\link{getNeighborhoodInMask}}}
}
\value{
List similar to the output of \code{\link{neighborhood}}
}
\description{
This function wraps the \code{\link{neighborhood}} to
get the matrix of data in the format of array indices (not values of data)
}
