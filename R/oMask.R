#' @title Get Mask from nifti object
#'
#' @description Wraps \code{getMask} from \code{ANTsR} for 
#' nifti objects
#' @param img Object of class nifti or character filename
#' @param ... arguments to \code{\link{getMask}} 
#' @export
#' @return Object of class \code{nifti}
#' @importFrom ANTsRCore getMask
oMask = function(img, ...){
  img = check_ants(img)
  mask = getMask(img = img, ...)
  rm(list = "img"); gc(); gc();
  mask = ants2oro(mask)
  gc();
  return(mask)
}