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
#' @param get.gradient logical indicating if a matrix of gradients (at the center voxel) 
#' should be returned in addition to the value matrix 
#' @param verbose Print diagnostic messages
#' @param run_gc Experimental - trying to run with gc(); for memory cleanup.
#' @param ... arguments other than \code{spatial.info} passed to 
#' \code{\link{getNeighborhoodInMask}}
#'
#' @return List similar to the output of \code{\link{getNeighborhoodInMask}}
#' @export
neighborhood = function(img, 
                        mask = NULL, 
                        radius = rep(1, 3), 
                        get.gradient = TRUE,
                        verbose = TRUE,
                        run_gc = TRUE,
                        ...){
  
  if (verbose) {
    message("Getting Image and Mask")
  }
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
  
  if (verbose) {
    message("Creating argument list")
  }  
  ##########################
  # Getting Neighborhood
  ##########################  
  dots = list(..., 
              image = img, 
              mask = mask, 
              radius = radius)
  dots$spatial.info = TRUE
  dots$get.gradient = FALSE

  if (verbose > 1) {
    print(dots)
  } 
  
  if (verbose) {
    message("Running getNeighborhood")
  } 
  grads = do.call(getNeighborhoodInMask, dots)
  # grads = getNeighborhoodInMask(
  #   image = img, 
  #   mask = mask, 
  #   radius = radius,
  #   spatial.info = TRUE, ...)
  
  ##########################
  # Getting Dimension
  ##########################  
  if (verbose) {
    message("Reordering output")
  }   
  L = reorder_neigh_indices(grads$indices, img.dim = dim(img))
  # inds = L$indices
  # tmp = L$tmp
  order_ind = L$order_ind

  grads$values = grads$values[, order_ind]
  grads$indices = grads$indices[order_ind,]
  if (get.gradient) {
    if (verbose) {
      message("Getting Gradient")
    }       
    dots$spatial.info = FALSE
    dots$get.gradient = TRUE
    if (verbose > 1) {
      print(dots)
    } 
    ggrads = do.call(getNeighborhoodInMask, dots)
    
    ggrads$gradients = ggrads$gradients[, order_ind]
    grads$gradients = ggrads$gradients
  }
  if (run_gc) {
    rm(list = c("img", "mask")); gc(); gc();
    rm(list = c("dots")); gc(); gc();
  }
  return(grads)
}
 