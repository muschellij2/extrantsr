#' Quantile Transform Voxel based on Neighborhood Distribution
#'
#' @inheritParams neighborhood
#'
#' @return An object of class \code{nifti}
#' @export
#'
#' @importFrom matrixStats rowAlls
#' @examples
#' nim = oro.nifti::nifti(array(rnorm(10^3), dim = rep(10, 3)))
#' mask = nim > -1
#' res = local_quantile_img(nim, mask = mask)
#' 
local_quantile_img = function(
  img, 
  mask = NULL, 
  radius = rep(1, 3), 
  verbose = TRUE) {
  
  grads = neighborhood(
    img = img, 
    mask = mask, 
    radius = radius,
    verbose = verbose,
    get.gradient = FALSE)
  mask = ants2oro(grads$mask)
  
  index = which(rowAlls(grads$offsets == 0))
  vals = t(grads$values)
  rm(grads); gc()
  
  image = vals[, index]
  if (any(is.na(image))) {
    warning("There are missing data in the original image!")
  }
  mat = vals <= image
  cdfs = rowMeans(mat, na.rm = TRUE)
  cdf_img = remake_img(vec = cdfs, img = mask, mask = mask)
  
  return(cdf_img)
}

