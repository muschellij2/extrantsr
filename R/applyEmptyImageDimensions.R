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
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}. 
#' @param reorient Should image be reoriented if a filename 
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' 
#' 
#' @export
#' @importFrom neurobase applyEmptyImageDimensions
setMethod("applyEmptyImageDimensions", "antsImage", 
          function(img, 
                   inds,
                   reorient = FALSE,
                   ...) {
            arr = as.array(img)
            res = neurobase::applyEmptyImageDimensions(
              img = arr, inds = inds, ...)
            res = ANTsR::as.antsImage(res)
            res = ANTsR::antsCopyImageInfo(reference = img, target = res)
            return(res)
          })