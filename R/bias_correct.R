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
#' @param dimension Dimension of the image (usually 3 or 4)
#' @param mask Mask to pass to \code{\link{n4BiasFieldCorrection}} 
#' @param ... additional arguments passed to 
#' \code{\link{n3BiasFieldCorrection}} or 
#'  \code{\link{n4BiasFieldCorrection}} 
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @import fslr
#' @export
bias_correct = function(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  outfile=NULL, 
  retimg = TRUE,
  reorient = FALSE,
  shrinkfactor = "4",
  dimension = 3, 
  mask = NULL,
  ...){
  correction = toupper(correction)
  correction = match.arg(correction, c("N3", "N4"))
  func = paste0(gsub("^N", "n", correction), "BiasFieldCorrection")
  
  outfile = check_outfile(outfile = outfile, 
                          retimg = retimg, 
                          fileext = '.nii.gz')
  
  img = check_ants(file, dimension = dimension)
#   imgn3 <- antsImageClone(img)
  if ( correction %in% c("N3", "n3")){
#     res = n3BiasFieldCorrection(img@dimension, img, imgn3, "4", ...)
    res = n3BiasFieldCorrection(img, 
                                downsampleFactor = shrinkfactor, 
                                ...)
  }
  if (correction %in% c("N4", "n4")){
    if (!is.null(mask)){
      mask = check_ants(x = mask)
    } else {
      mask = NA
    }
#     print(class(mask))
#     funclist = list(d=img@dimension, i=img, o=imgn3, s = shrinkfactor, ...)
#     res = do.call(func, funclist)
    res = n4BiasFieldCorrection(img = img, 
                                mask = mask, 
                                shrinkFactor = as.numeric(shrinkfactor), 
                                ...)
  }
#   if (correction == "N4_Field"){
#     funclist = list(d=img@dimension, i=img, o=imgn3, s = shrinkfactor, ...)
#     res = n4BiasCorrect_WithField(funclist)
#   }  
  imgn3 = res
  antsImageWrite( imgn3, filename = outfile)
  
  if (retimg){
    x = readnii(outfile, reorient = reorient)
  } else {
    x = outfile
  }
  return(x)
}