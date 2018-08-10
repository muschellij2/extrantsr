#' @title Multi-Atlas Label Fusion Registration
#'
#' @description Takes in an input file and template images with 
#' a set of template structures and registers them
#' @param infile Input Image file
#' @param template.images Template Images to register 
#' to \code{infile}
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param inverted Should the MALF be inverted 
#' (infile to template then use inverse transforms)
#' 
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
#' @param rerun_registration Should \code{\link{malf}} be run again
#' if transforms already exist?
#' @export
#' @return List of registrations and
#' output files
#' @import utils
malf_registration <- function(
  infile, template.images, template.structs,
  inverted = FALSE,
  typeofTransform = "SyN",  
  interpolator = "NearestNeighbor",
  keep_images = TRUE, 
  outfiles = NULL,
  outprefix = NULL,
  verbose = TRUE,
  rerun_registration = TRUE,
  ...){
  
  dot_args = list(...)
  template.images = checkimg(template.images)
  template.structs = checkimg(template.structs)
  
  nimgs = length(template.images)
  stopifnot(nimgs == length(template.structs))
  if (is.null(outfiles)) {
    outfiles = sapply(seq(nimgs), function(x){
      tempfile(fileext = paste0("_", x, ".nii.gz"))
    })
  }  
  if (keep_images) {
    stopifnot(length(outfiles) == nimgs)
  }
  
  if (verbose) {
    message("# Doing Registrations")
    pb = txtProgressBar(min = 0, max = nimgs, style = 3)     
  }
  # all.regs = NULL
  all.regs = vector(mode = "list", length = length(template.images))
  if (is.null(outprefix)) {
    outprefix = tempfile()
  }  
  dir.create(dirname(outprefix), showWarnings = FALSE,
             recursive = TRUE)
  for (iimg in seq_along(template.images)) {
    timage = template.images[[iimg]]
    tstruct = template.structs[[iimg]]
    ofile = outfiles[[iimg]]
    i_outprefix = paste0(outprefix, "_", iimg, "_")
    
    #########################################
    # Workup for Rerun
    #########################################    
    reg = transformlist_from_outprefix(outprefix = i_outprefix)
    if (all(file.exists(reg$fwdtransforms)) && !rerun_registration) {
      args = list(
        typeofTransform = typeofTransform,
        interpolator = interpolator,
        transformlist = reg$fwdtransforms,
        ret_ants = TRUE)
      if (!inverted) {
        args$moving = tstruct
        args$fixed = infile
      } else {
        args$fixed = tstruct
        args$moving = infile
      }
      img = do.call(ants_apply_transforms, args = args)
      write_nifti(img, filename = ofile)
      reg$outfile = ofile
      reg$interpolator = args$interpolator
      reg$typeofTransform = args$typeofTransform
      reg$retimg = FALSE
      reg$other_interpolator = dot_args$other_interpolator
      reg$invert_interpolator = dot_args$invert_interpolator
      
    } else {
      if (!inverted) {
        reg = registration(
          filename = timage, 
          template.file = infile,
          outfile = tempfile(fileext = ".nii.gz"),
          typeofTransform = typeofTransform,
          interpolator = interpolator,
          retimg = FALSE,
          other.files = tstruct,
          other.outfiles = ofile,
          outprefix = i_outprefix,
          remove.warp = FALSE,
          verbose = verbose > 1,
          ...)
      } else {
        reg = registration(
          # switch template.file and filename
          template.file = timage, 
          filename = infile,
          outfile = tempfile(fileext = ".nii.gz"),
          typeofTransform = typeofTransform,
          interpolator = interpolator,
          retimg = FALSE,
          # instead of using other.file and other.files
          invert.file = tstruct,
          invert.native.fname = ofile,
          outprefix = i_outprefix,
          remove.warp = FALSE,
          verbose = verbose > 1,
          ...)      
      }
    }
    # all.regs = c(all.regs, reg)
    all.regs[[iimg]] = reg
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