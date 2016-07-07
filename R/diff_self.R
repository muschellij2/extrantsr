#' @title Flipped Difference Image
#' @description This function flips the image over the left/right axis, then registers 
#' the flipped image to the original image and then takes a difference
#' @param img Object of class \code{nifti}, \code{ANTsR}, or \code{character}
#' @param swapdim Should the image be reoriented to RPI then flipped, registered,
#' then changed back?
#' @param verbose Print diagnostic messages
#' @return Object of class \code{nifti}
#' @export
diff_self <- function(img, 
                      swapdim = TRUE,
                      verbose = TRUE){
  img = checkimg(img)
  if (swapdim) {
    L = rpi_orient(file = file)
    img = checkimg(L$img)
  }
  
  rotate.img = checkimg(img)
  original.img = check_ants(img)
  
  ofile = tempfile(fileext = ".nii.gz")
  res = fslswapdim(rotate.img,
                    a = "-x", 
                    retimg = FALSE,
                    outfile = ofile,
                    verbose = verbose)
  res = NULL
  rotate.img = check_ants(ofile)
  mytx <- antsRegistration(
    fixed = original.img,
    moving = rotate.img,
    typeofTransform = "SyN",
    verbose = verbose)
  flipped.reg <- antsApplyTransforms(
    fixed = original.img,
    moving = rotate.img,
    transformlist = mytx$fwdtransforms)
  
  img.return <- original.img - flipped.reg
  if (swapdim) {
    file = checkimg(img.return)
    file = reverse_rpi_orient(file = file, 
                           convention = L$convention,
                           orientation = L$orientation,
                           verbose = verbose)
    return(file)  
  } else {
    img.return <- ants2oro(img.return)
  }
  return(img.return)
}