#' @title Apply MALF Registrations
#'
#' @description Takes in an input file, set of registrations, 
#' and a set of template structures and applies the registrations,
#' with a different interpolator if necessary
#' @param infile Input Image file
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param keep_images Keep the \code{template.structs} in 
#' \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}, can be different than original MALF
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{ants_apply_transforms}}
#' @export
#' @import fslr
#' @return The output filename or the nifti image or list of registrations and
#' output file
apply_malf_regs <- function(
  infile,
  regs,
  template.structs,
  keep_images = TRUE, 
  outfiles = NULL,
  interpolator = NULL,
  verbose = TRUE,
  ...){
  
  stopifnot(length(template.structs) == length(regs))
  all_out = vector(mode = "list", length = length(regs))
  
  if (!is.null(outfiles)) {
    stopifnot(length(outfiles) == length(regs))
    stopifnot(inherits(outfiles, "character"))
  }
  for (ireg in seq_along(regs)) {
    
    reg = regs[[ireg]]
    ############################################
    # Can pass in new interpolator
    ############################################    
    interp = interpolator
    if (is.null(interpolator)) {
      interp = reg$interpolator
    }
    ############################################
    # Get Transformations 
    ############################################
    fwdtransforms = regs$fwdtransforms
    struct = template.structs[[ireg]]
    out = ants_apply_transforms(
      fixed = infile, 
      moving = struct, 
      transformlist = fwdtransforms,
      interpolator = interp, 
      verbose = verbose,
      ...)
    all_out[[ireg]] = out
    if (!is.null(outfiles)) {
      writenii(out, filename = outfiles[[ireg]])
    }
  }
  
  return(all_out)
}