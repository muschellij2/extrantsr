#' @title Center of Gravity for Multiple Components/Areas
#' @description Find center of gravity for each compoent of an image
#' @param img Object of class \code{\link{nifti}} or array
#' @param k Minimum number of voxels for a cluster/component.  
#' See \code{\link{ants_bwlabel}}
#' @return Matrix of 3 columns of dimension indices.
#' @export 
group_xyz2 = function(img, k = 1) {
  les_xyz = ants_bwlabel(img, k = k, binary = FALSE)
  les_levs = sort(unique(les_xyz[les_xyz != 0]))
  if (length(les_levs) <= 1) {
    return(t(xyz(les_xyz)))
  } else {
    les_xyz = t(sapply(les_levs, function(x) {
      xyz(les_xyz == x)
    }))
    return(les_xyz)
  }
  return(les_xyz)
}