#' @title Multi-Registration
#'
#' @description Takes a list of images and registers them to a template, 
#' keeping the registration information in a list.
#' @param infiles Input image files
#' @param template.file Template image to register 
#' to
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}  
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param outprefix Character path of where the warp files and transformations
#' should be stored.
#' @param retimg Return image to user using \code{\link{readNIfTI}}
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{ants_regwrite}}
#' @export
#' @import fslr
#' @return Output list of registered images and transformations
multi_reg <- function(infiles, 
                      template.file,
                      outfiles = NULL,
                      typeofTransform = "SyN", 
                      interpolator = "Linear", 
                      outprefix = NULL,
                      retimg = TRUE,
                      verbose = TRUE,
                      ...){
  
  infiles = checkimg(infiles)
  template.file = checkimg(template.file)
  nimgs = length(infiles)
  
  if (is.null(outfiles)) {
    outfiles = sapply(seq(nimgs), function(x){
      tempfile(fileext = ".nii.gz")
    })
  }
  
  if (is.null(outprefix)) {
    outprefix = sapply(seq(nimgs), function(x){
      tempfile()
    })
  }
  
  outprefix = c(outprefix, 
                rep(outprefix, nimgs - length(outprefix))
  )
  
  if (verbose) {
    cat("# Doing Registrations\n")
    pb = txtProgressBar(min = 0, max = nimgs, style = 3) 
  }
  
  oimgs = vector(mode = "list", length = nimgs)
  for (iimg in seq_along(infiles)) {
    in_image = infiles[iimg]
    ofile = outfiles[[iimg]]
    oprefix = outprefix[[iimg]]
    res = registration(
      filename = in_image, 
      outfile = ofile,
      retimg = retimg,
      template.file = template.file,
      typeofTransform = typeofTransform, 
      interpolator = interpolator, 
      verbose = verbose,
      outprefix = oprefix,
      remove.warp = FALSE,
      ...)
    oimgs[[iimg]] = res
    if (verbose) {
      setTxtProgressBar(pb, iimg)
    }
  }
  if (verbose) {
    close(pb)
  }
  return(oimgs)
}