#' @rdname pixdim-methods
#' @title Extract Image pixdim attribute 
#' @description Gets pixdim from an antsImage
#' 
#' @aliases pixdim,antsImage-method
#' @param object is an antsImage
#' 
#' @importFrom oro.nifti pixdim
#' @importMethodsFrom oro.nifti pixdim
#' 
#' @return Vector of numeric values
#' @examples 
#' library(ANTsRCore)
#' fn <- getANTsRData( "r16" )
#' fi <- antsImageRead( fn )
#' pixdim(fi)
setMethod("pixdim", "antsImage", function(object){
  pdim = c(0, antsGetSpacing(object))
  pdim = c(pdim, rep(1, length = max(0, 8 - length(pdim))))
  return(pdim)
})
