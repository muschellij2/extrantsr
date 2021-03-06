% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/otropos.R
\name{otropos2}
\alias{otropos2}
\title{Run Atropos for nifti objects}
\usage{
otropos2(img, mask = NULL, smoothing_factor = 0.2, radius = c(1, 1, 1), ...)
}
\arguments{
\item{img}{Images to segment.}

\item{mask}{Mask of Image}

\item{smoothing_factor}{amount of smoothing in Markov random Field. 
Increasing this number causes more smoothing whereas decreasing 
the number lessens the smoothing.}

\item{radius}{the mrf neighborhood.  Length must equal the number
of dimensions of \code{img}}

\item{...}{arguments to pass to \code{\link{otropos}}}
}
\description{
Runs \code{\link{otropos}} but has different argument
names to be more consistent with \code{extrantsr}
}
\seealso{
\code{\link{otropos}}, \code{\link{atropos}}
}
