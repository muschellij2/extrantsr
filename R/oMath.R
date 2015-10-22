#' @title Perform iMath on nifti objects
#'
#' @description Images are converted \code{nifti} objects
#' (from the oro.nifti package), an operation is performed using 
#' \code{\link{iMath}}, and the result is a \code{nifti} object.
#' @param img Object of class \code{nifti} 
#' @param ... Additional arguments passed to \code{\link{iMath}}
#' @export
#' @import ANTsR
#' @return Object of class \code{nifti}
oMath <- function(img, ...){
  img = check_ants(img)
  res = iMath(img = img, ...)
  res = ants2oro(res)
  return(res)
}