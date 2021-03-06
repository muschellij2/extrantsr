% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bias_correct.R
\name{bias_correct}
\alias{bias_correct}
\alias{bias_correct_ants}
\title{N3 or N4 Correct}
\usage{
bias_correct(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  outfile = NULL,
  retimg = TRUE,
  reorient = FALSE,
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...
)

bias_correct_ants(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{file}{(character) image to be manipulated}

\item{correction}{(character) N3 or N4 correction?}

\item{outfile}{(character) resultant image name (optional)}

\item{retimg}{(logical) return image of class nifti}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link{readnii}}.}

\item{shrinkfactor}{Shrink factor passed to 
\code{\link{n3BiasFieldCorrection}}}

\item{mask}{Mask to pass to \code{\link{n4BiasFieldCorrection}}}

\item{verbose}{print diagnostic output.}

\item{...}{additional arguments passed to 
\code{\link{n3BiasFieldCorrection}} or 
 \code{\link{n4BiasFieldCorrection}}}
}
\value{
If \code{retimg} then object of class nifti.  Otherwise,
Result from system command, depends if intern is TRUE or FALSE.
}
\description{
This function wraps ANTsR bias field corrections and returns
nifti objects
}
