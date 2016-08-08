#' @title Get Scaled Neighborhood of Voxels from an Image
#' @description This function wraps the \code{\link{neighborhood}} to
#' provided scaled neighborhood or its ranks
#' @param img Object of class \code{\link{nifti}}, character or \code{antsImage}
#' @param mask Binary image of class \code{nifti}, 
#' \code{antsImage}, \code{character}.  Only neighborhoods inside the mask 
#' will be taken
#' @param radius vector of length 3 for number of voxels to go in each direction.
#' Default is 27 neighbors (including voxel at center).  
#' Passed to \code{\link{getNeighborhoodInMask}}.
#' @param method If pearson, scaling done on original data.  If spearman,
#' the ranks.
#' @param center Should the data be centered
#' @param scale Should the data be scaled
#' @param verbose print diagnostic messages
#' #' 
#' @param verbose Print diagnostic messages
#'
#' @return List similar to the output of \code{\link{getNeighborhoodInMask}}
#' @export
scaled_neighborhood = function(img, mask = NULL, 
                               radius = rep(1, 3), 
                               method = c("pearson", "spearman"),
                               center = TRUE,
                               scale = TRUE,
                               verbose = TRUE){
  method = match.arg(method)
  col_scale = function(x, center = TRUE, scale = TRUE){
    if (center) {
      cm = colMeans(x, na.rm = TRUE)
    } else {
      cm = rep(0, length(x))
    }
    if (scale) {
      csd = colSds(x, center = cm,
                 na.rm = TRUE)
    } else {
      csd = rep(1, length(x))
    }
    if (center || scale) {
      x = t( (t(x) - cm) / csd )
    } else {
      return(x)
    }
  }  
  
  grads = neighborhood(
    img = img, 
    mask = mask, 
    radius = radius,
    get.gradient = FALSE, 
    verbose = verbose)

  if (method == "spearman") {
    grads$values = colRanks(grads$values, 
                            ties.method = "average",
                            preserveShape = TRUE)
  }
  grads$values = col_scale(grads$values, center = center, scale = scale)
  return(grads)
}
