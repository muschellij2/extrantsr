
#' Four-Dimensional Bias-Field Correction
#'
#' @param img Image for N4 to be run, an antsImage
#' @param verbose Print diagnostic messages
#'
#' @return An object of class \code{antsImage}
#' @export
n4_4d = function(img, verbose = TRUE) {
  img = check_ants(img)

  spacing = antsGetSpacing(img)
  direction = antsGetDirection(img)
  origin = antsGetOrigin(img)
  pixeltype = img@pixeltype
  components = (img@components > 1)
  
  dims = dim(img)
  out = array(NA, dim = dims)
  n_times = dims[4]
  
  if (is.na(n_times) || n_times == 0) {
    stop("Not a 4D image for N4!")
  }

  img = as.array(img)
  
  
  if (verbose) {
    
    pb = txtProgressBar(max = n_times,
                        style = 3)
  }
  for (i_time in seq_len(n_times)) {
    rimg = img[,,,i_time, drop = TRUE]
    rimg = as.antsImage(rimg, 
                       spacing = spacing[1:3], 
                       direction = direction, 
                       origin = origin[1:3],
                       pixeltype = pixeltype,
                       components = components)
    n4 = n4BiasFieldCorrection(rimg)
    out[,,,i_time] = as.array(n4)
    rm(list = c("n4")); gc(verbose = FALSE);
    if (verbose) {
      setTxtProgressBar(pb, value = i_time)
    }
  }
  if (verbose) {
    close(pb)
  }
  outimg = as.antsImage(
    out, 
    spacing = spacing, 
    direction = direction, 
    origin = origin,
    pixeltype = pixeltype,
    components = components)
  return(outimg)
  
}