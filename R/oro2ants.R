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
#' @export
#' @return Object of class \code{nifti}.
ants2oro <- function(img, 
                     reorient = FALSE,
                     reference = NULL,
                     ...){
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
    fname = tempants(img)
    img = readnii(fname, reorient = reorient)
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
#' @export
#' @import fslr
#' @import ANTsR
#' @return Object of class \code{antsImage}
oro2ants <- function(img, reference = NULL){
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
    fname = checkimg(img)
    stopifnot(file.exists(fname))
    img = antsImageRead(fname)
    return(img)
  }
  if ( is.antsImage(img) ) {
    return(img)    
  }   
  stop("img not class nifti or antsImage")
  return(NULL)
}