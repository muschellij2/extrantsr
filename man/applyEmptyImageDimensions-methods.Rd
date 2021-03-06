% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applyEmptyImageDimensions.R
\docType{methods}
\name{applyEmptyImageDimensions,antsImage-method}
\alias{applyEmptyImageDimensions,antsImage-method}
\alias{applyEmptyImageDimensions,nifti-method}
\title{Apply Subsetting from Empty Image Dimensions}
\usage{
\S4method{applyEmptyImageDimensions}{antsImage}(img, inds, reorient = FALSE, ...)
}
\arguments{
\item{img}{image, nifti object, or array}

\item{inds}{indices of subset from \code{\link{getEmptyImageDimensions}} or
\code{\link{dropEmptyImageDimensions}}.}

\item{reorient}{Should image be reoriented if a filename}

\item{...}{not used}
}
\value{
Object of class \code{nifti} or \code{array} if \code{nifti}
is not supplied
}
\description{
Simple wrapper for subsetting an image with indices, 
dropping empty dimensions.
}
