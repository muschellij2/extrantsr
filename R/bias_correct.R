#' @title N3 or N4 Correct
#' @description This function wraps ANTsR bias field corrections and returns
#' nifti objects
#' @param file (character) image to be manipulated
#' @param correction (character) N3 or N4 correction?
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.
#' @param shrinkfactor Shrink factor passed to 
#' \code{\link{N3BiasFieldCorrection}}
#' @param ... additional arguments passed to 
#' \code{\link{N3BiasFieldCorrection}}
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
bias_correct = function(
  file,
  correction = c("N3", "N4", "N4_Field"),
  outfile=NULL, 
  retimg = FALSE,
  reorient = FALSE,
  shrinkfactor = "4",
  ...){
  correction = match.arg(correction, c("N3", "N4", "N4_Field"))
  func = paste0(correction, "BiasFieldCorrection")
  
  if (retimg){
    if (is.null(outfile)) {
      outfile = tempfile()
      outfile = paste0(nii.stub(outfile), '.nii.gz')      
    }
  } else {
    stopifnot(!is.null(outfile))
  }
  
  if (inherits(file, "antsImage")){
    img = file
  } else {
    img <- antsImageRead(file, 3)
  }
  imgn3 <- antsImageClone(img)
  if ( correction == "N3"){
    res = N3BiasFieldCorrection(img@dimension, img, imgn3, "4", ...)
  }
  if (correction == "N4"){
    funclist = list(d=img@dimension, i=img, o=imgn3, s = shrinkfactor, ...)
    res = do.call(func, funclist)
  }
  if (correction == "N4_Field"){
    funclist = list(d=img@dimension, i=img, o=imgn3, s = shrinkfactor, ...)
    res = N4BiasCorrect_WithField(funclist)
  }  
  
  antsImageWrite( imgn3, filename = outfile)
  
  if (retimg){
    x = readNIfTI(outfile, reorient = reorient)
  } else {
    x = outfile
  }
  return(x)
}