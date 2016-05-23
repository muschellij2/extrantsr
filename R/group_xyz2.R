#' @title Label Connected Components
#' @description Label a binary image with connected components
#' @param img Object of class \code{\link{nifti}} or array
#' @param k Minimum number of voxels for a cluster/component.
#' @return Object of class \code{\link{nifti}}
#' @importFrom neuroim connComp3D
#' @export 
label_mask = function(img, k = 1){
  bin_mask = as(img > 0, "array")
  cc = neuroim::connComp3D(bin_mask)
  cc$index[cc$size < k] = 0
  les_xyz = cc$index
  if (is.nifti(img)) {
    les_xyz = niftiarr(img, les_xyz)
  }
  return(les_xyz)
}

#' @title Center of Gravity for Multiple Components/Areas
#' @description Find center of gravity for each compoent of an image
#' @param img Object of class \code{\link{nifti}} or array
#' @param k Minimum number of voxels for a cluster/component.  
#' See \code{\link{label_mask}}
#' @return Matrix of 3 columns of dimension indices.
#' @export 
group_xyz2 = function(img, k = 1) {
  les_xyz = label_mask(img, k = k)
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
