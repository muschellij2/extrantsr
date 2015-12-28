#' @name checkimg-methods
#' @description Ensures the output to be a character filename (or vector) from an input
#' image or \code{nifti}.  
#' @title Force object to filename  
#' @docType methods 
#' @aliases checkimg
#' @rdname checkimg-methods
#' @aliases checkimg,antsImage-method
#' @import fslr
#' @export
setMethod("checkimg", "antsImage", function(file, ...) { 
  file = tempants(file, ...)
  return(file)
})