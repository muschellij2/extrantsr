#' @title Convert Between antsImage and nifti objects
#'
#' @description NIfTI data can be converted between \code{antsImage} 
#' (from the ANTsR package) and nifti S4 objects.
#' @param img Object of class antsImage 
#' @param reorient Reorientation passed to \code{\link{readnii}}
#' @param reference Object of class \code{\link{nifti}} to copy header
#' information
#' @param ... Arguments passed to \code{\link{copyNIfTIHeader}} if 
#' reference is set and \code{img} is \code{antsImage}
#' @param cleanup temporary files are deleted after they are read in
#' @param drop_dim Should \code{\link{drop_img_dim}} be run after reading? 
#' Passed to \code{\link{readnii}}
#' @export
#' @return Object of class \code{nifti}.
#' @importFrom neurobase copyNIfTIHeader readnii mask_img check_outfile writenii datatyper
#' @importFrom neurobase zscore_img same_dims robust_window remake_img xyz zero_pad
#' @importFrom neurobase check_mask_fail niftiarr nii.stub
ants2oro <- function(img, 
                     reorient = FALSE,
                     reference = NULL,
                     ...,
                     cleanup = TRUE,
                     drop_dim = TRUE){
  if ( is.antsImage(img) | is.character(img) ) {
    if (is.antsImage(img) & !is.null(reference)) {
      #######
      # allow for reference, but need nifti
      #######
      if (!is.null(reference)) {
        if (!is.nifti(reference)) {
          stop("reference must be of class nifti")
        }
      }
      #######
      # Turn into array and then make nifti
      #######
      img = as.array(img)
      img = as(img, Class = "array")
      img = copyNIfTIHeader(img = reference, arr = img, ...)
      gc();
      return(img)
    }
    ############################
    # Otherwise write image to disk
    # And read in nifti
    ############################    
    if (is.antsImage(img)) {
      remove = TRUE
    }

    fname = tempants(img)
    img = readnii(fname, reorient = reorient, drop_dim = drop_dim)
    if (remove & cleanup & file.exists(fname)) {
      file.remove(fname)
    }
    gc();
    return(img)
  }
  if ( is.nifti(img) ) {
    return(img)
  }
  stop("img not class nifti or antsImage or character")
  return(NULL)
}

#' @title Convert Between nifti and antsImage    
#'
#' @description NIfTI data can be converted between \code{nifti} 
#' (from the oro.nifti package) and \code{antsImage} objects.
#' @param img Object of class \code{nifti} 
#' @param reference Object of class \code{antsImage} to 
#' copy header information (origin, spacing, direction) (experimental)
#' @param cleanup temporary files are deleted after they are read in
#' @export
#' @import fslr
#' @import ANTsR
#' @return Object of class \code{antsImage}
oro2ants <- function(img, reference = NULL,
                     cleanup = TRUE){
  if (!is.null(reference)) {
    if (is.antsImage(reference)) {
      img = as(img, Class = "array")
      aimg = as.antsImage(img)
      aimg = antsCopyImageInfo(
        target = aimg, 
        reference = reference)
      return(aimg)
    }
  }
  if (  is.nifti(img) | is.character(img) ) {
    if (is.nifti(img)){
      remove = TRUE
    }
    fname = checkimg(img)
    stopifnot(file.exists(fname))
    img = antsImageRead(fname)
    if (remove & cleanup) {
      file.remove(fname)
    }    
    return(img)
  }
  if ( is.antsImage(img) ) {
    return(img)    
  }   
  stop("img not class nifti or antsImage")
  return(NULL)
}
