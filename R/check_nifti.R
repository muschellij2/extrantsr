#' @rdname check_nifti-methods
#' @aliases check_nifti,character-method
#' @docType methods 
#' @title Check if nifti image or read in a nifti image
#' @description Simple check to see if input is character or of 
#' class nifti
#' @return nifti object or array if allow.array=TRUE and x is an array
#' @seealso \link{readnii}
#' @param x character path of image or 
#' an object of class nifti, or array
#' @param reorient (logical) passed to \code{\link{readnii}} 
#' if the image
#' is to be re-oriented
#' @param allow.array (logical) Are array types allowed (TRUE) or
#' should there be an error if the object is not character or class
#' nifti.
#' @export
#' @importFrom neurobase check_nifti
setMethod("check_nifti", "antsImage", function(x, 
                                               reorient=FALSE, 
                                               allow.array=FALSE) { 
  ants2oro(x, reorient = reorient)
})
