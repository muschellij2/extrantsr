#' @rdname check_ants-methods
#' @aliases check_ants,nifti-method
#' @importFrom ANTsRCore check_ants
#' @export
setMethod("check_ants", "nifti", function(x) { 
  x = oro2ants(x)
  return(x)
})