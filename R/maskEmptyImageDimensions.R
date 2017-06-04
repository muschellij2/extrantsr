#' @rdname maskEmptyImageDimensions-methods
#' @aliases maskEmptyImageDimensions,nifti-method
#' @export
#' 
#' @docType methods 
#' 
#' @title Apply Masking from Empty Image Dimensions
#' @description Simple wrapper for replacing indices with a value
#' @param img image, nifti object, or array
#' @param inds indices of subset from \code{\link{getEmptyImageDimensions}} or
#' \code{\link{dropEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' @param mask.value Value to replace voxels outside the mask.
#' @param ... not used
#' @return Object of class \code{nifti} or \code{array} if \code{nifti}
#' is not supplied
#' 
#' @note \code{apply_empty_dim} is a shorthand for 
#' \code{maskEmptyImageDimensions} with all the same arguments.
#' @seealso \code{\link{getEmptyImageDimensions}}, 
#' \code{\link{dropEmptyImageDimensions}} 
#' @export
#' @importFrom neurobase maskEmptyImageDimensions
setMethod("maskEmptyImageDimensions", "antsImage", 
          function(img, 
                   inds,
                   reorient = FALSE,
                   mask.value = 0,
                   ...) {
            arr = as.array(img)
            res = neurobase::maskEmptyImageDimensions(
              img = arr, inds = inds, 
              mask.value = mask.value, ...)
            res = ANTsRCore::as.antsImage(res)
            res = ANTsRCore::antsCopyImageInfo(reference = img, target = res)
            return(res)
          })