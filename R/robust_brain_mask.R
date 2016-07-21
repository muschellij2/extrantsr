#' @title Robust Brain Mask from Template
#'
#' @description Masks the brain from scan using a rigid body transformation
#' of the template mask to the native brain space and then inflating the mask
#' to adjust for mis-registration.
#' @param file File for masking - either filename or class nifti
#' @param template.file Template to warp to original image space
#' @param template.mask Mask of template to use as rough brain mask.  If
#' \code{template.file} is specified, but \code{template.mask} is not,
#' then \code{fslbin(file=template.file)} is performed.
#' @param ret_mask Return mask of robust brain
#' @param nvoxels Number of voxels in nxnxn direction to inflate, see
#' \code{\link{fslmaths.help}} for \code{-kernel boxv n -dilM}.
#' @param typeofTransform Transformation for template to image, passed to
#' \code{\link{ants_regwrite}}.
#' @param verbose Print diagnostic progress
#' @param ... Arguments passed to \code{\link{ants_regwrite}}
#' @export
#' @return Object of class nifti or vector of indices
robust_brain_mask <- function(file,
                        template.file,
                        template.mask = NULL,
                        nvoxels = 7,
                        ret_mask = FALSE,
                        typeofTransform = "Rigid",
                        verbose = TRUE,
                        ...){

  file = checkimg(file)
  ofile = tempfile(fileext = '.nii.gz')
  if (missing(template.file)){
    message("Potential atlases are at\n ")
    message(paste0('system.file("scct_unsmooth.nii.gz", package="ichseg")\n'))
    message(paste0('file.path( fsldir(), "data/standard", ',
               '"MNI152_T1_1mm_brain.nii.gz")\n'))
    stop("Need template.file specified!")
  }
  template.file = checkimg(template.file)
  if (is.null(template.mask)){
    template.mask = fslbin(file=template.file, retimg=TRUE)
  }
  template.mask = checkimg(template.mask)
  if (verbose){
    message("# Putting Template mask in native space\n")
  }
  ret = ants_regwrite(filename = template.file, template.file = file,
                      typeofTransform=typeofTransform,
                      other.files = template.mask,
                      outfile = tempfile(fileext = '.nii.gz'),
                      other.outfiles = ofile, retimg = FALSE,
                      remove.warp = TRUE,
                      verbose = verbose,
                      ...)

  if (verbose){
    message("# Binarizing Template mask in native space\n")
  }
  endofile = tempfile(fileext = ".nii.gz")
  fslbin(ofile, outfile = endofile)
  if (nvoxels > 0){
    if (verbose){
      message("# Inflating Template mask in native space\n")
    }
    fslmaths(file = endofile, outfile = endofile,
           opts = paste0("-kernel boxv ", nvoxels, " -dilM"))
  }
  if (ret_mask){
    newimg = readnii(endofile, reorient = FALSE)
  } else {
    newimg = fslmask(file = file, mask = endofile, retimg = TRUE)
  }

  return(newimg)
}
