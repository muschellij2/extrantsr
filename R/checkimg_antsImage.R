#' @name checkimg-methods
#' @title Force object to filename 
#' @description Ensures the output to be a character filename (or vector) from an input
#' image or \code{nifti}.  
#' @docType methods 
#' @rdname checkimg-methods
#' @aliases checkimg,antsImage-method
#' @param file character or \code{nifti} object
#' @param ... options passed to \code{\link{tempimg}} 
#' @export
setMethod(f = "checkimg", signature(file = "antsImage"), definition = function(file, ...) { 
  file = tempants(file, ...)
  return(file)
})