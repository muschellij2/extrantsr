#' @title Get Neighborhood of Voxels from an Image
#' @description This function wraps the \code{\link{getNeighborhoodInMask}} to
#' be more friendly for R and \code{nifti} objects
#' @param img Object of class \code{\link{nifti}}, character or \code{antsImage}
#' @param mask Binary image of class \code{nifti}, 
#' \code{antsImage}, \code{character}.  Only neighborhoods inside the mask 
#' will be taken
#' @param radius vector of length 3 for number of voxels to go in each direction.
#' Default is 27 neighbors (including voxel at center).  
#' Passed to \code{\link{getNeighborhoodInMask}}.
#' @param ... arguments other than \code{spatial.info} passed to 
#' \code{\link{getNeighborhoodInMask}}
#'
#' @return List similar to the output of \code{\link{getNeighborhoodInMask}}
#' @export
neighborhood = function(img, mask = NULL, 
                        radius = rep(1, 3), ...){
  
  #####!! need verbose option
  img = check_ants(img)
  img.dim = dim(img)
  
  if (is.null(mask)) {
    mask = array(1, 
              dim = img.dim)  
    mask = as.antsImage(mask)
    mask = antsCopyImageInfo(
      target = mask, 
      reference = img)    
  }
  
  mask = check_ants(mask)
  
  ##########################
  # Getting Neighborhood
  ##########################  
  grads = getNeighborhoodInMask(
    image = img, 
    mask = mask, 
    radius = radius,
    spatial.info = TRUE, ...)
  
  ##########################
  # Getting Dimension
  ##########################  
  grads$indices = grads$indices + 1
  inds = grads$indices
  tmp = array(0, 
              dim = img.dim)
  tmp[inds] = 1
  inds = which(tmp > 0)
  
  order_ind = order(inds)
  grads$values = grads$values[, order_ind]
  
  grads$indices = grads$indices[order_ind,]
  
  return(grads)
}