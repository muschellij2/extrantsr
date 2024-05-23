#' @export
#' @method window_img antsImage
window_img.antsImage = function(x, ...) {
  arr = as.array(x)
  arr = neurobase::window_img(arr, ...)
  arr = as.antsImage(arr, reference = x)
  return(arr)
}
