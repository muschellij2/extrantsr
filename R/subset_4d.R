#' @name subset_4d-methods
#' @docType methods 
#' @aliases subset_4d 
#' @title Subset a 4D image
#' @description Subsets an \code{\link{antsImage}} or \code{\link{nifti}}
#' object from 4D
#' @return An \code{\link{antsImage}} or \code{\link{nifti}} depending on
#' input
#' @param img character path of image or 
#' an object of class nifti, or antsImage
#' @param ind subset indexers for the 4th dimension (usually time)
#' @param ... Arguments passed to \code{\link{copyNIfTIHeader}} or 
#' \code{\link{antsCopyImageInfo}}
#' @export 
#' @examples 
#' n = 20
#' x = nifti(array(rnorm(n^3*10), dim = c(n, n, n, 10)))
#' ind = 2:3
#' subset_4d(x, ind)
#' subset_4d(x, ind = 1) 
#' @author John Muschelli \email{muschellij2@@gmail.com} 
setGeneric("subset_4d", function(img, ind, ...) {
  standardGeneric("subset_4d")
})

#' @rdname subset_4d-methods
#' @aliases subset_4d,character-method
#'  
#' @export
setMethod("subset_4d", "character", function(img, ind, ...) { 
  
  img = readnii(img)
  arr = .subset_4d(img, ind)
  newimg = copyNIfTIHeader(img, arr, ...)
  return(newimg)
})

#' @rdname subset_4d-methods
#' @aliases subset_4d,nifti-method
#'  
#' @export
setMethod("subset_4d", "nifti", function(img, ind, ...) { 

  arr = .subset_4d(img, ind)
  newimg = copyNIfTIHeader(img, arr, ...)
  return(newimg)
})

#' @rdname subset_4d-methods
#' @aliases subset_4d,antsImage-method
#'  
#' @export
setMethod("subset_4d", "antsImage", function(img, ind, ...) { 
  
  arr = .subset_4d(img, ind)
  newimg = as.antsImage(arr)
  newimg = antsCopyImageInfo(img, 
                             newimg, 
                             ...)   
  return(newimg)
})



#' @rdname subset_4d-methods
.subset_4d = function(img, ind) {
  dimg = dim(img)
  if (length(dimg) != 4){
    stop("not a 4D image")
  }
  arr = as.array(img)[,,,ind]
  return(arr)
}
