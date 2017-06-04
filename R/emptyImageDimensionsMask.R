#' @rdname emptyImageDimensionsMask-methods
#' @aliases emptyImageDimensionsMask,antsImage-method
#' 
#' @docType methods 
#' 
#' @title Make Mask from Empty Image Dimensions
#' @description Make a mask of an image that has all irrelevant
#' values 
#' @name emptyImageDimensionsMask
#' @param img nifti object
#' @param ... Arguments to be passed to \code{\link{getEmptyImageDimensions}}.
#' @param reorient Should image be reoriented if a filename?
#' 
#' @return Object of class \code{nifti}, with binary values
#' 
#' @note \code{empty_dim_mask} is a shorthand for \code{emptyImageDimensionsMask}
#' with all the same arguments.qQac vgfrew
#' 
#' @seealso \code{\link{getEmptyImageDimensions}}  
#' @export
#' @importFrom neurobase emptyImageDimensionsMask
setMethod("emptyImageDimensionsMask", "antsImage", 
          function(img, 
                   ...,
                   reorient = FALSE) { 
            arr = as.array(img)
            res = neurobase::emptyImageDimensionsMask(
              img = arr, 
              ..., 
              reorient = reorient
              )
            res = ANTsRCore::as.antsImage(res)
            res = ANTsRCore::antsCopyImageInfo(reference = img, target = res)
            return(res)
          })
