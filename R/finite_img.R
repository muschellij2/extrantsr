#' @name finite_img-methods
#' @aliases finite_img,antsImage-method
#' 
#' @title Finite Image
#' @description Simple wrapper for setting non-finite values to zero 
#' @return image object 
#' @param img character path of image or 
#' an object of class \code{nifti}, or list of images
#' @param replace Value to replace non-finite values to
#' @export 
#' @importFrom neurobase finite_img
setMethod("finite_img", "antsImage", function(img, replace = 0) { 
  img[ !is.finite(as.array(img)) ] = replace
  return(img)
})
