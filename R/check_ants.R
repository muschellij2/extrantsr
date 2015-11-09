#' @name check_ants-methods
#' @docType methods 
#' @aliases check_ants 
#' @title Check if antsImage or read in
#' @description Simple check to see if input is character, list, \code{nifti},
#' or class \code{antsImage}
#' @return antsImage object
#' @seealso \code{\link{antsImageRead}}
#' @param x character path of image or 
#' an object of class antsImage
#' @param dimension (numeric) passed to 
#' \code{\link{antsImageRead}} if the image
#' is read in
#' @export 
#' @import methods
#' @author John Muschelli \email{muschellij2@@gmail.com}  
setGeneric("check_ants", function(x, dimension = 3) {
  standardGeneric("check_ants")
})

#' @rdname check_ants-methods
#' @aliases check_ants,antsImage-method
#' @export
setMethod("check_ants", "antsImage", function(x, dimension = 3) { 
    x2 = antsImageClone(x)
    return(x2)
})

#' @rdname check_ants-methods
#' @aliases check_ants,nifti-method
#' @export
setMethod("check_ants", "nifti", function(x, dimension = 3) { 
  x = oro2ants(x)
  return(x)
})

#' @rdname check_ants-methods
#' @aliases check_ants,character-method
#'  
#' @export
setMethod("check_ants", "character", function(x, dimension = 3) { 
  ### add vector capability
  if (length(x) > 1){
    file = lapply(x, check_ants, dimension = dimension)
    return(file)
  } else {
    img = antsImageRead(x, dimension = dimension)
    return(img)
  }
})


#' @rdname check_ants-methods
#' @aliases check_ants,list-method
#' @export
setMethod("check_ants", "list", function(x, dimension = 3) { 
  ### add vector capability
  file = lapply(x, check_ants, dimension = dimension)
  return(file)
})




