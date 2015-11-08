#' @title Multi-Registration
#'
#' @description Takes a list of images and registers them to a template, 
#' keeping the registration information in a list.
#' @param infile Input image file
#' @param template.images Template images to register 
#' to \code{infile}
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param keep_images Keep the \code{template.structs} in 
#' \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}} 
#' @param outfile Fused output filename
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