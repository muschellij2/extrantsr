#' @export
#' @method mask_img antsImage
#' @importFrom neurobase mask_img 
mask_img.antsImage = function(img, mask, allow.NA = TRUE){
  mask = neurobase::ensure_array(mask)
  res = as.antsImage(img * mask, reference = img )
  res
}