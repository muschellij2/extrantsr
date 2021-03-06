% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scaled_neighborhood.R
\name{scaled_neighborhood}
\alias{scaled_neighborhood}
\title{Get Scaled Neighborhood of Voxels from an Image}
\usage{
scaled_neighborhood(
  img,
  mask = NULL,
  radius = rep(1, 3),
  method = c("pearson", "spearman"),
  center = TRUE,
  scale = TRUE,
  verbose = TRUE
)
}
\arguments{
\item{img}{Object of class \code{\link{nifti}}, character or \code{antsImage}}

\item{mask}{Binary image of class \code{nifti}, 
\code{antsImage}, \code{character}.  Only neighborhoods inside the mask 
will be taken}

\item{radius}{vector of length 3 for number of voxels to go in each direction.
Default is 27 neighbors (including voxel at center).  
Passed to \code{\link{getNeighborhoodInMask}}.}

\item{method}{If pearson, scaling done on original data.  If spearman,
the ranks.}

\item{center}{Should the data be centered}

\item{scale}{Should the data be scaled}

\item{verbose}{print diagnostic messages}
}
\value{
List similar to the output of \code{\link{getNeighborhoodInMask}}
}
\description{
This function wraps the \code{\link{neighborhood}} to
provided scaled neighborhood or its ranks
}
