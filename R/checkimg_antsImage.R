#' @name checkimg-methods
#' @title Force object to filename 
#' @description Ensures the output to be a character filename (or vector) from an input
#' image or \code{nifti}.  
#' @docType methods 
#' @rdname checkimg-methods
#' @aliases checkimg,antsImage-method
#' @param file character or \code{antsImage} object
#' @param allow_array allow arrays to be passed in
#' @param ... options passed to \code{\link{tempimg}} 
#' @export
#' @importFrom neurobase checkimg
setMethod(f = "checkimg", signature(file = "antsImage"), 
          definition = function(file, allow_array = FALSE, ...) { 
  file = tempants(file, ...)
  gc(); gc();
  return(file)
})