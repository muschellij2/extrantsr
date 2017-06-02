#' @export
#' @method window_img antsImage
#' @importFrom neurobase window_img 
window_img.antsImage = function(x, ...) {
  arr = as.array(x)
  arr = neurobase::window_img(arr, ...)
  arr = ANTsRCore::as.antsImage(arr, reference = x)
  return(arr)
}
