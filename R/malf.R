#' @title Multi-Atlas Label Fusion
#'
#' @description Takes in an input file and template images with 
#' a set of template structures and creates a label fusion
#' @param infile Input Image file
#' @param template.images Template Images to register 
#' to \code{infile}
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param keep_images Keep the \code{template.structs} in 
#' \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param outfile Fused output filename
#' @param retimg Return Image to user using \code{\link{readnii}}
#' @param func function to combine labels.  See \code{\link{stat_img}}.
#' @param ties.method If \code{func = "mode"}, then this is passed to 
#' \code{\link{stat_img}}. 
#' @param keep_regs Keep list of registrations.  If \code{TRUE}, then
#' \code{remove.warp = FALSE} in \code{\link{registration}}
#' @param outprefix passed to \code{\link{registration}} if 
#' \code{keep_regs = TRUE}
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}  
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}} 
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{malf_registration}}, which
#' really are optios for \code{\link{registration}} 
#' @export
#' @import fslr
#' @return The output filename or the nifti image or list of registrations and
#' output file
malf <- function(
  infile, template.images, template.structs,
  keep_images = TRUE, 
  outfiles = NULL,
  outfile = NULL, 
  retimg = TRUE,
  func = "mode",
  ties.method = "first",
  keep_regs = FALSE,
  outprefix = NULL,
  interpolator = "NearestNeighbor",
  typeofTransform = "SyN",
  verbose = TRUE,
  ...){
  
  
  
  have.outfile = !is.null(outfile)
  outfile = fslr::check_outfile(outfile = outfile, retimg = retimg, 
                          fileext = "")
  
  ##############################
  # Run all the registrations
  ##############################
  L = malf_registration(
    infile = infile, 
    template.images = template.images, 
    template.structs = template.structs,
    typeofTransform = typeofTransform,
    interpolator = interpolator,    
    keep_images = keep_images, 
    outfiles = outfiles,
    outprefix = outprefix,
    verbose = verbose,
    ...)
  outfiles = L$outfiles
  all.regs = L$regs
  
  if (verbose) {
    cat("# Reading in Files\n")
  }
  if (func == "mode") {
    outimg = stat_img(imgs = outfiles, func = func, ties.method = ties.method)
  } else {
    outimg = stat_img(imgs = outfiles, func = func)
  }
  #     oimgs = lapply(outfiles, readnii, reorient = FALSE)
  #     mat = sapply(oimgs, c)
  #     outimg = niftiarr(oimgs[[1]], rowMeans(mat))
  if (have.outfile) {
    writenii(outimg, filename = outfile)
  }
  if (!keep_regs) {
    if (retimg) {
      return(outimg)
    } else {
      return(outfile)
    }
  } else {
    if (!retimg) {
      outimg = outfile
    } 
    L = list(regs = all.regs, 
             outimg = outimg, 
             statistic = func)
    return(L)
  }
}