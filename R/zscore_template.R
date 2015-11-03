#' @title Create Z-score-to-Template Image
#' @description Registers an image to a template, subtracts a mean and sd image, and
#' will return the image either in template space or native.
#' @param img Image to be z-scored
#' @param template.file Template file to be registered to.  An affine registration
#' will be done usually, so if the image is skull-stripped, use a skull-stripped
#' template and vice versa
#' @param mean.img Mean image to be subtracted
#' @param sd.img SD image to be divded
#' @param outfile Output filename.  Will use \code{\link{writenii}} to write
#' @param typeofTransform Transform for registration.  See \code{\link{antsRegistration}}
#' @param interpolator Interpolator to use, See \code{\link{antsRegistration}}
#' @param robust Should See \code{\link{robust_window}} be used on the image
#' @param native Should a native-space image (default) or template-space image
#' be returned
#' @param verbose Print diagnostic output
#'
#' @return Object of class \code{nifti}
#' @export
zscore_template <- function (img,
                             template.file,
                             mean.img,
                             sd.img,
                             outfile = NULL,
                             typeofTransform = "SyN",
                             interpolator = "Linear",
                             robust = TRUE,
                             native = TRUE,
                             verbose = TRUE
                             ) {
  
  mean.img = check_nifti(mean.img)
  sd.img = check_nifti(sd.img)
  
  outprefix = tempfile()
  
  ss.res = ants_regwrite(filename = img, 
                         correct = FALSE, 
                         outfile = NULL, 
                         retimg = TRUE, 
                         typeofTransform = typeofTransform,
                         template.file = template.file, 
                         interpolator = interpolator,
                         remove.warp = FALSE,
                         outprefix = outprefix,
                         verbose = verbose)

  #############################
  # Image in template space
  #############################
  z.img = niftiarr(ss.res, (ss.res - mean.img) / sd.img)
  z.img[ !is.finite(z.img)] = NA
  if (!native){
    z.img = robust_window(z.img)
    if (!is.null(outfile)) {
      writenii(z.img, filename = outfile)
    }    
    return(z.img)
  }
  
  inv.trans = c(
    paste0(outprefix, "0GenericAffine.mat"),
    paste0(outprefix, "1InverseWarp.nii.gz")
  )
  
  ants.ss = check_ants(img)
  ants.z = oro2ants(z.img)
  
  zout = antsApplyTransforms(fixed = ants.ss, 
                             moving = ants.z, 
                             transformlist = inv.trans)

  
  
  znative = ants2oro(zout)
  if (robust) {
    znative = robust_window(znative)
  }
  
  if (!is.null(outfile)) {
    writenii(znative, filename = outfile)
  }

  return(znative)
}