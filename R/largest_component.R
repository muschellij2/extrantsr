#' @title Get Largest Component
#'
#' @description Wrapper for \code{oMath("GetLargestComponent")}
#' 
#' @param img Object of class \code{nifti} 
#' @param ... Additional arguments passed to \code{\link{iMath("GetLargestComponent")}}
#' @param retfile logical to indicate if an \code{antsImage} should be returned
#' (useful for chaining)
#' @export
#' @return Object of class \code{nifti}
largest_component <- function(img, ..., retfile = FALSE){
  res = oMath(img = img, operation = "GetLargestComponent", 
              ..., retfile = retfile)
  return(res)
}


