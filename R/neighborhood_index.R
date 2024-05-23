
#' @title Get Neighborhood Matrix of Indices from an Image
#' @description This function wraps the \code{\link{neighborhood}} to
#' get the matrix of data in the format of array indices (not values of data)
#' 
#' @param img Object of class \code{\link{nifti}}, character or \code{antsImage}
#' @param ... arguments other than \code{get.gradient} passed to 
#' \code{\link{getNeighborhoodInMask}}
#'
#' @return List similar to the output of \code{\link{neighborhood}}
#' @export
neighborhood_index = function(
  img, 
  ...){
  
  img = check_ants(img)
  vdim = voxdim(img)
  dimg = dim(img)
  nvoxels = prod(dimg)
  arr = array(seq(nvoxels), dim = dimg)
  rm(list = c("nvoxels"));
  
  run_img = as.antsImage(arr, reference = img)
  rm(list = "arr"); gc();
  
  res = neighborhood(
    img = run_img, 
    get.gradient = FALSE,
    ...)
  gc();
  return(res)
}
