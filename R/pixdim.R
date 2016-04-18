#' @docType methods
#' @rdname pixdim-methods
#' @title Extract Image pixdim attribute 
#' @description Gets pixdim from an antsImage
#' @name pixdim-methods
#' @aliases pixdim,antsImage-method
#' @param object is an antsImage
#' @return Vector of numeric values
#' @export
setMethod("pixdim", "antsImage", function(object){
  pdim = c(0, antsGetSpacing(object))
  pdim = c(pdim, rep(1, length = max(0, 8 - length(pdim))))
  return(pdim)
})