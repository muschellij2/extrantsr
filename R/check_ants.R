#' @title Check if antsImage or read in
#' @description Simple check to see if input is character, list, \code{nifti},
#' or class \code{antsImage}
#' @return antsImage object
#' 
#' @rdname check_ants-methods
#' @aliases check_ants,nifti-method
#' @importFrom ANTsRCore check_ants
#' @export
setMethod("check_ants", "nifti", function(x) { 
  x = oro2ants(x)
  return(x)
})