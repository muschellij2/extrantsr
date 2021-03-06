% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ss_bias.R
\name{ss_bias}
\alias{ss_bias}
\title{Skull Strip then Bias Field Correct}
\usage{
ss_bias(
  filename,
  maskfile = NULL,
  outfile = tempfile(fileext = ".nii.gz"),
  skull_strip = TRUE,
  bet.opts = "",
  betcmd = "bet",
  correct = TRUE,
  correction = "N3",
  shrinkfactor = "4",
  retimg = TRUE,
  reorient = FALSE,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{filename}{filename to be processed}

\item{maskfile}{filename of masked to be passed in, otherwise bet is performed}

\item{outfile}{output filename with extension (e.g. \code{.nii.gz})}

\item{skull_strip}{should skull stripping be done}

\item{bet.opts}{options for fslbet}

\item{betcmd}{bet command to be used see \code{\link{fslbet}}}

\item{correct}{should n3 correction be done stripping be done}

\item{correction}{correction method used see \code{\link{bias_correct}}}

\item{shrinkfactor}{correction method used see \code{\link{n3BiasFieldCorrection}}}

\item{retimg}{(logical) return image of class nifti}

\item{reorient}{(logical) If retimg, should file be reoriented when read in?
Passed to \code{\link{readnii}}.}

\item{verbose}{Diagnostic output}

\item{...}{passed to \code{\link{bias_correct}}}
}
\value{
Filename of output file or object of class nifti
}
\description{
This function wraps \code{fslbet} and 
\code{bias_correct}, performing skull stripping first
}
\seealso{
\code{\link{bias_ss}}
}
