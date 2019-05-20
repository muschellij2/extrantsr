#' @title Zero pads an image
#' 
#' @param img Array, class \code{\link{nifti}}, or 
#' \code{\link{antsImage}}
#' @param ... Options to \code{\link[neurobase]{zero_pad}}
#' @return Object of class nifti, array, or \code{\link{antsImage}}
#' @export
zero_pad = function(img, 
                    ...){
  if (is.antsImage(img)) {
    arr = as.array(img)
    res = neurobase::zero_pad(arr, ...)
    res = ANTsRCore::as.antsImage(object = res, reference = img)
  } else {
    res = neurobase::zero_pad(img, ...)
  }
  return(res)
}