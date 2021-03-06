% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/apply_malf_regs.R
\name{apply_malf_regs}
\alias{apply_malf_regs}
\title{Apply MALF Registrations}
\usage{
apply_malf_regs(
  infile,
  regs,
  template.structs,
  keep_images = TRUE,
  outfiles = NULL,
  inverted = FALSE,
  interpolator = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{infile}{Input Image file}

\item{regs}{List of registrations from \code{\link{malf}} or 
\code{\link{malf_registration}}, each element must have 
\code{fwdtransforms} and \code{interpolator}. Same length as 
\code{template.structs}}

\item{template.structs}{Template gold standards to apply 
registration into \code{infile} space}

\item{keep_images}{Keep the \code{template.structs} in 
\code{infile} space}

\item{outfiles}{Output filenames for  \code{template.structs} in 
\code{infile} space}

\item{inverted}{Should the MALF be inverted (inverse transforms are used
instead of forward)}

\item{interpolator}{interpolation done for 
\code{\link{antsApplyTransforms}}, can be different than original MALF}

\item{verbose}{Print diagnostic output}

\item{...}{Arguments to be passed to \code{\link{ants_apply_transforms}}}
}
\value{
The output filename or the nifti image or list of registrations and
output file
}
\description{
Takes in an input file, set of registrations, 
and a set of template structures and applies the registrations,
with a different interpolator if necessary
}
