
#' @title Reorder Voxel Neighborhood
#' @description The output from \code{\link{getNeighborhoodInMask}} gets reordered
#' to match that which R uses for array to indices.
#' @param indices Indices from \code{\link{getNeighborhoodInMask}}
#' @param img.dim Image dimension from which neighborhood was taken
#'
#' @return List of new indices, the ordering, and a binary array which has a 
#' 1 where the indices are located
#' @export
#'
reorder_neigh_indices = function(indices,
  img.dim){
  # Due to change in output
  # https://github.com/ANTsX/ANTsR/commit/706148aa994d414c9efd76e9292ae99351e2c4be
  if (min(indices) == 0) {
    inds = indices + 1
  } else {
    inds = indices
  }
  tmp = array(0, dim = img.dim)
  tmp[inds] = 1
  inds = which(tmp > 0)
  L = list(indices = inds,
           order_ind = order(inds),
           array = tmp)
  return(L)
}