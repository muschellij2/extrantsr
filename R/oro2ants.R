#' @title Convert Between antsImage and oro.nifti  
#'
#' @description NIfTI data can be converted between \code{antsImage} 
#' (from the ANTsR package) and nifti S4 objects.
#' @param img Object of class antsImage 
#' @param reorient Reorientation passed to \code{\link{readNIfTI}}
#' @export
#' @return Object of class \code{nifti}
ants2oro <- function(img, 
                     reorient = FALSE){
  if (inherits(x, "antsImage") | inherits(x, "character")) {
    fname = tempants(img)
    img = readNIfTI(fname, reorient = reorient)
    return(img)
  }
  if (!inherits(x, "nifti")) {
    return(x)
  }
  stop("img not class nifti or antsImage")
  return(NULL)
}

#' @title Convert Between oro.nifti and antsImage    
#'
#' @description NIfTI data can be converted between \code{nifti} 
#' (from the oro.nifti package) and \code{antsImage} objects.
#' @param img Object of class \code{nifti} 
#' @param reorient Reorientation passed to \code{\link{readNIfTI}}
#' @export
#' @import fslr
#' @return Object of class \code{antsImage}
oro2ants <- function(img){
  if (inherits(x, "nifti") | inherits(x, "character")) {
    fname = checkimg(img)
    img = antsImageRead(fname, dimension = 3)
  }
  if (!inherits(x, "antsImage")) {
    return(x)    
  }   
  stop("img not class nifti or antsImage")
  return(img)
}