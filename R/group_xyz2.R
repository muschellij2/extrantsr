#' @title Center of Gravity for Multiple Components/Areas
#' @description Find center of gravity for each compoent of an image
#' @param img Object of class \code{\link{nifti}} or array
#' @param k Minimum number of voxels for a cluster/component.  
#' See \code{\link{ants_bwlabel}}
#' @return Matrix of 3 columns of dimension indices.
#' @export 
group_xyz2 = function(img, k = 1) {
  img = check_ants(img)
  labs = labelClusters(
    img,
    minClusterSize = 1,
    fullyConnected = TRUE)
  les_xyz = as.array(labs)
  tab = table(c(les_xyz))
  levs = names(tab[tab >= k])
  levs = as.numeric(levs)
  les_levs = levs[ levs > 0 ]  


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
