#' @export
#' @method ensure_array antsImage
#' @importFrom neurobase ensure_array
ensure_array.antsImage = function(img){
  img = as.array(img)
  img = as(img, "array")  
  return(img)
}
