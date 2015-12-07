
#' @title Gaussian smooth image using ANTsR
#' @description This function calls \code{smoothIamge} to smooth an image and either
#' saves the image or returns an object of class nifti
#' @param file (character) image to be smoothed
#' @param sigma (numeric) sigma (in mm) of Gaussian kernel for smoothing
#' @param mask (character) optional mask given for image
#' @param smooth_mask (logical) Smooth mask?  If TRUE, the masked image 
#' will be divided by the smoothed mask.
#' @param verbose (logical) print out command before running
#' @param ... additional arguments passed to \code{\link{smoothImage}}.
#' @return Object of class \code{nifti}
#' @examples
#' system.time({
#' library(oro.nifti)
#' x = array(rnorm(1e6), dim = c(100, 100, 100))
#' img = nifti(x, dim= c(100, 100, 100), 
#' datatype = convert.datatype()$FLOAT32, cal.min = min(x), 
#' cal.max = max(x), pixdim = rep(1, 4))
#' s.img = smooth_image(img, retimg=TRUE)
#' })
#' @export
smooth_image <- function(
  file,
  sigma=10, 
  mask=NULL, 
  smooth_mask = TRUE,
  verbose = TRUE,
  ...){
  
  file = check_ants(file)
  if (!is.null(mask) ) {
    mask = check_ants(mask)
    file = maskImage(img.in = file, img.mask = mask)
  }
  
  sm_file = smoothImage(inimg = file, sigma = sigma, ...)
  if (!is.null(mask)  & smooth_mask ) {
    # smoothing mask
    sm_mask = smoothImage(inimg = mask, sigma = sigma, ...)
    # dividing smoothed mask
    sm_file = sm_file / sm_mask
    # remask
    sm_file = maskImage(img.in = sm_file, img.mask = mask)
  }
  sm_file = ants2oro(sm_file)
  return(sm_file)
}