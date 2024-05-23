#' @title Check if antsImage or read in
#' @description Simple check to see if input is character, list, \code{nifti},
#' or class \code{antsImage}
#' @return antsImage object
#' @param x character path of image or 
#' an object of class antsImage
#' @param ... arguments passed to \code{\link{oro2ants}}
#' @rdname check_ants-methods
#' @aliases check_ants,nifti-method
#' @export
setMethod("check_ants", "nifti", function(x, ...) { 
  x = oro2ants(x, ...)
  return(x)
})