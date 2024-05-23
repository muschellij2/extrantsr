
#' Four-Dimensional Bias-Field Correction
#'
#' @param img Image for N4 to be run, an antsImage
#' @param verbose Print diagnostic messages
#' @param correction Correction to do (N4 or N3)
#' @param ... arguments to pass to \code{\link{bias_correct_ants}}
#'
#' @return An object of class \code{antsImage}
#' @export
n4_4d = function(img, verbose = TRUE, correction = "N4", ...) {
  img = check_ants(img)
  
  spacing = ANTsR::antsGetSpacing(img)
  direction = ANTsR::antsGetDirection(img)
  origin = ANTsR::antsGetOrigin(img)
  pixeltype = ANTsR::pixeltype(img)
  components = ANTsR::components(img)
  
  dims = dim(img)
  out = array(NA, dim = dims)
  n_times = dims[4]
  
  if (is.na(n_times) || n_times == 0) {
    stop("Not a 4D image for N4!")
  }
  
  img = as.array(img)
  if (verbose) {
    pb = txtProgressBar(
      max = n_times,
      style = 3)
  }
  for (i_time in seq_len(n_times)) {
    rimg = img[,,,i_time, drop = TRUE]
    rimg = ANTsR::as.antsImage(
      object = rimg, 
      spacing = spacing[1:3], 
      direction = direction, 
      origin = origin[1:3],
      pixeltype = pixeltype,
      components = components)
    n4 = bias_correct_ants(
      file = rimg, 
      correction = correction, ...)
    
    out[,,,i_time] = as.array(n4)
    rm(list = c("n4")); gc(verbose = FALSE);
    if (verbose) {
      setTxtProgressBar(pb, value = i_time)
    }
  }
  if (verbose) {
    close(pb)
  }
  outimg = ANTsR::as.antsImage(
    object = out, 
    spacing = spacing, 
    direction = direction, 
    origin = origin,
    pixeltype = pixeltype,
    components = components)
  return(outimg)
  
}