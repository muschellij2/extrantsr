#' @title Image Time Series to list
#' @description Turns a 4D time series image to a list of 3D images
#' @param imgs object of class \code{\link{nifti}} with 4 dimensions, 
#' aka a 4D time series
#' @param copy_nifti Should \code{nifti} objects be returned (\code{TRUE}) or 
#' simply arrays (\code{FALSE}).  Should only be used for slight speed up when
#' array is adequate
#' @param warn Should a warning be printed if object is not class
#' \code{\link{nifti}}
#'
#' @return List of images
#' @note If the object is not of class \code{\link{nifti}} or have
#' 4 dimensions, then the object is returned
#' @export
img_ts_to_list = function(imgs, copy_nifti = TRUE, warn = TRUE) {
  
  if (length(dim(imgs)) == 4) {
    if (is.nifti(imgs)) {
      L = apply(imgs, 4, list)
      if ( copy_nifti ) {
        L = lapply(L, function(x) {
          copyNIfTIHeader(imgs, x[[1]])
        })
      } else {
        L = lapply(L, function(x) {
          x[[1]]
        })
      }
    } else {
      if (warn) {
        warning("Object is not of class nifti")
      }
      return(imgs)
    }
  } else {
    return(imgs)
  }
}