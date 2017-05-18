#' @rdname applyEmptyImageDimensions-methods
#' @aliases applyEmptyImageDimensions,nifti-method
#' @export
#' 
#' @docType methods 
#' @title Apply Subsetting from Empty Image Dimensions
#' @description Simple wrapper for subsetting an image with indices, 
#' dropping empty dimensions.  
#' 
#' @param img image, nifti object, or array
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' 
#' 
#' @export
#' @importFrom neurobase applyEmptyImageDimensions
setMethod("applyEmptyImageDimensions", "antsImage", 
          function(img, ...) {
            arr = as.array(img)
            res = neurobase::applyEmptyImageDimensions(
              img = arr, ...)
            res = ANTsR::as.antsImage(arr, reference = img)
            return(res)
          })