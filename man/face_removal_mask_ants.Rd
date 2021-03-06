% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/face_removal_mask_ants.R
\name{face_removal_mask_ants}
\alias{face_removal_mask_ants}
\alias{deface_image_ants}
\title{Face Removal Mask using ANTs Registration}
\usage{
face_removal_mask_ants(
  file,
  template = NULL,
  face_mask = NULL,
  outfile = NULL,
  ...,
  retimg = FALSE
)

deface_image_ants(file, ...)
}
\arguments{
\item{file}{input image}

\item{template}{Template image  to register input image to.  Set 
to \code{NULL} (recommended)  if want to use from 
\url{https://github.com/poldracklab/pydeface}.
Alternatively, use \code{\link{mni_fname}}.}

\item{face_mask}{Mask of image, in same space as \code{template}.
Set to \code{NULL} (recommended) if want to use from 
\url{https://github.com/poldracklab/pydeface}. 
Alternatively, use \code{mni_face_fname} from \code{fsl}.}

\item{outfile}{Output file name}

\item{...}{additional arguments to pass to 
\code{\link{registration}}}

\item{retimg}{(logical) return image of class nifti}
}
\value{
An image or filename depending on \code{retimg}
}
\description{
Face Removal Mask using ANTs Registration
}
\examples{
\donttest{
if (fslr::have_fsl()) {
   file = "~/Downloads/sample_T1_input.nii.gz"
   if (file.exists(file)) {
       mask = face_removal_mask_ants(file = file,
         template = NULL, face_mask = NULL)
       image = mask_img(file, mask)
   }
}
}
}
