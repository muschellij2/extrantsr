#' @title Reapply MALF Operation
#'
#' @description Takes in an input file, set of registrations, 
#' and a set of template structures and applies the registrations,
#' with a different interpolator if necessary, and then combines output
#' @param infile Input Image file
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}} 
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{ants_apply_transforms}}
#' @export
#' @return The output filename or the nifti image or list of registrations and
#' output file
reapply_malf <- function(
  infile,
  regs,
  template.structs,
  keep_images = FALSE, 
  outfiles = NULL,  
  outfile = NULL, 
  retimg = TRUE,
  func = "mode",
  ties.method = "first",
  interpolator = NULL,
  verbose = TRUE,
  ...){
  
  ofiles = apply_malf_regs(
    infile = infile,
    regs = regs,
    template.structs = template.structs,
    keep_images = keep_images, 
    outfiles = outfiles,
    interpolator = interpolator,
    verbose = verbose,
    ...)
  
  if (verbose) {
    message("# Reading in Files\n")
  }
  if (func == "mode") {
    outimg = stat_img(imgs = ofiles, 
                      func = func, ties.method = ties.method)
  } else {
    outimg = stat_img(imgs = ofiles, func = func)
  }
  
  if (!is.null(outfile)) {
    writenii(outimg, filename = outfile)
  }
  
  if (retimg) {
    return(outimg)
  } else {
    return(outfile)
  }
  stop("Shouldn't be here - bug?")  
}