#' @title Flip NifTI Image
#'
#' @description This image will flip x, y, or z direction
#' @param img nifti object or character filename
#' @param x (logical) Flip x direction
#' @param y (logical) Flip y direction
#' @param z (logical) Flip z direction
#' @param ... Arguments passed to \code{\link{check_nifti}}
#' @export
#' @return Object of class nifti
flip_img <- function(img, x = FALSE, y = FALSE, z = FALSE, ...){
  img = check_nifti(img, ...)
  d = dim(img)
  if (x){
    for (i in seq(d[3])){
      x = img[,,i]
      x = x[,ncol(x):1]
      img[,,i] = x
    }
  }
  if (y){
    for (i in seq(d[3])){
      x = img[,,i]
      x = x[nrow(x):1,]
      img[,,i] = x
    }
  }  
  if (y){
    for (i in seq(d[1])){
      x = img[i,,]
      x = x[,ncol(x):1]
      img[i,,] = x
    }
  }
  return(img)
}