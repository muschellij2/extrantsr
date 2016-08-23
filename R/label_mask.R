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
