#' @title N3 or N4 Correct
#' @description This function wraps ANTsR bias field corrections and returns
#' nifti objects
#' @param file (character) image to be manipulated
#' @param correction (character) N3 or N4 correction?
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param shrinkfactor Shrink factor passed to 
#' \code{\link{n3BiasFieldCorrection}}
#' @param mask Mask to pass to \code{\link{n4BiasFieldCorrection}} 
#' @param verbose print diagnostic output.
#' @param ... additional arguments passed to 
#' \code{\link{n3BiasFieldCorrection}} or 
#'  \code{\link{n4BiasFieldCorrection}} 
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
bias_correct = function(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  outfile = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...){
  
  outfile = neurobase::check_outfile(
    outfile = outfile, 
    retimg = retimg, 
    fileext = ".nii.gz")
  
  res = bias_correct_ants(
    file = file,
    correction = correction,
    shrinkfactor = shrinkfactor,
    mask = mask,
    verbose = verbose,
    ...)
  
  ANTsRCore::antsImageWrite(res, filename = outfile)
  rm(list = c("res")); 
  for (i in 1:10) {
    gc()
  }  
  
  if (retimg) {
    x = neurobase::readnii(outfile, reorient = reorient)
  } else {
    x = outfile
  }
  
  return(x)
}

#' @rdname bias_correct
#' @export
bias_correct_ants = function(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...){
  correction = toupper(correction)
  correction = match.arg(correction, c("N3", "N4"))
  
  img = check_ants(file)
  
  if (correction %in% c("N3", "n3")) {
    res = ANTsRCore::n3BiasFieldCorrection(
      img = img, 
      downsampleFactor = shrinkfactor, 
      ...)
  }
  if (correction %in% c("N4", "n4")) {
    if (!is.null(mask)) {
      mask = check_ants(x = mask)
    } else {
      mask = NA
    }
    
    res = ANTsRCore::n4BiasFieldCorrection(
      img = img, 
      mask = mask, 
      shrinkFactor = as.numeric(shrinkfactor), 
      verbose = verbose,
      ...)
  }
  rm(list = c("img")); 
  for (i in 1:10) {
    gc()
  }  
  return(res)
}
