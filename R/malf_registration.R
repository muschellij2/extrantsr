#' @title Multi-Atlas Label Fusion Registration
#'
#' @description Takes in an input file and template images with 
#' a set of template structures and registers them
#' @param infile Input Image file
#' @param template.images Template Images to register 
#' to \code{infile}
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}  
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param keep_images Keep the \code{template.structs} in 
#' \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param outprefix passed to \code{\link{registration}} if 
#' \code{keep_regs = TRUE}
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{registration}}
#' @export
#' @import fslr
#' @return List of registrations and
#' output files
#' @import utils
malf_registration <- function(
  infile, template.images, template.structs,
  typeofTransform = "SyN",  
  interpolator = "NearestNeighbor",
  keep_images = TRUE, 
  outfiles = NULL,
  outprefix = NULL,
  verbose = TRUE,
  ...){
  
  template.images = checkimg(template.images)
  template.structs = checkimg(template.structs)
  
  nimgs = length(template.images)
  stopifnot(nimgs == length(template.structs))
  if (keep_images) {
    stopifnot(length(outfiles) == nimgs)
  }
  if (is.null(outfiles)) {
    outfiles = sapply(seq(nimgs), function(x){
      tempfile(fileext = ".nii.gz")
    })
  }
  
  if (verbose) {
    cat("# Doing Registrations\n")
    pb = txtProgressBar(min = 0, max = nimgs, style = 3)     
  }
  all.regs = NULL
  if (is.null(outprefix)) {
    outprefix = tempfile()
  }  
  for (iimg in seq_along(template.images)) {
    timage = template.images[[iimg]]
    tstruct = template.structs[[iimg]]
    ofile = outfiles[[iimg]]
    i_outprefix = paste0(outprefix, "_", iimg)
    reg = registration(filename = timage, 
                       outfile = tempfile(fileext = ".nii.gz"),
                       typeofTransform = typeofTransform,
                       interpolator = interpolator,
                       retimg = FALSE,
                       template.file = infile,
                       other.files = tstruct,
                       other.outfiles = ofile,
                       outprefix = i_outprefix,
                       remove.warp = FALSE,
                       verbose = verbose,
                       ...)
    all.regs = c(all.regs, reg)
    if (verbose) {
      setTxtProgressBar(pb, iimg)
    }
    rm(list = "reg")
    for (i in 1:10) {
      gc()
    }
  } # end loop
  if (verbose) {
    close(pb)
  }
  L = list(regs = all.regs, 
           outfiles = outfiles)
  for (i in 1:10) {
    gc()
  }  
  return(L)

}