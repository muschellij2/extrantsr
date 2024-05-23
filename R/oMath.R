#' @title Perform iMath on nifti objects
#'
#' @description Images are converted \code{nifti} objects
#' (from the oro.nifti package), an operation is performed using 
#' \code{\link{iMath}}, and the result is a \code{nifti} object.
#' @param img Object of class \code{nifti} 
#' @param ... Additional arguments passed to \code{\link{iMath}}
#' @param retfile logical to indicate if an \code{antsImage} should be returned
#' (useful for chaining)
#' @export
#' @return Object of class \code{nifti}
oMath <- function(img, ..., retfile = FALSE){
  img = check_ants(img)
  res = iMath(img = img, ...)
  rm(list = "img"); gc(); gc();
  if (retfile) {
    return(res)
  }
  res = ants2oro(res)
  return(res)
}
