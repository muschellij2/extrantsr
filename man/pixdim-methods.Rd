% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pixdim.R
\name{pixdim,antsImage-method}
\alias{pixdim,antsImage-method}
\title{Extract Image pixdim attribute}
\usage{
\S4method{pixdim}{antsImage}(object)
}
\arguments{
\item{object}{is an antsImage}
}
\value{
Vector of numeric values
}
\description{
Gets pixdim from an antsImage
}
\examples{
library(ANTsR)
fn <- getANTsRData( "r16" )
fi <- antsImageRead( fn )
pixdim(fi)
}
