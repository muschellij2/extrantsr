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
#' @param keep_regs Keep list of registrations.  If \code{TRUE}, then
#' \code{remove.warp = FALSE} in \code{\link{registration}}
#' @param outprefix passed to \code{\link{registration}} if 
#' \code{keep_regs = TRUE}
#' @param verbose Print diagnostic output
#' @param ... Arguments to be passed to \code{\link{registration}}
#' @export
#' @import fslr
#' @return The output filename or the nifti image
malf <- function(infile, template.images, template.structs,
                keep_images = TRUE, 
                outfiles = NULL,
                outfile = NULL, 
                retimg = TRUE,
                func = "mean",
                keep_regs = FALSE,
                outprefix = NULL,
                verbose = TRUE,
                ...){
    
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
    have.outfile = !is.null(outfile)
    outfile = check_outfile(outfile = outfile, retimg = retimg, 
                            fileext = "")
    
    if (verbose) {
      cat("# Doing Registrations\n")
      pb = txtProgressBar(min = 0, max = nimgs, style = 3)     
    }
    all.regs = NULL
    for (iimg in seq_along(template.images)) {
      timage = template.images[[iimg]]
      tstruct = template.structs[[iimg]]
      ofile = outfiles[[iimg]]
      if (is.null(outprefix)) {
        outprefix = tempfile()
      }
      reg = registration(filename = timage, 
                    outfile = tempfile(fileext = ".nii.gz"),
                    retimg = FALSE,
                    template.file = infile,
                    other.files = tstruct,
                    other.outfiles = ofile,
                    outprefix = outprefix,
                    remove.warp = !keep_regs,
                    verbose = verbose,
                    ...)
      if (keep_regs) {
        all.regs = c(all.regs, reg)
      }
      if (verbose) {
        setTxtProgressBar(pb, iimg)
      }
    }
    if (verbose) {
      close(pb)         
    }
    if (verbose) {
      cat("# Reading in Files\n")
    }
    outimg = stat_img(imgs = outfiles, func = func)
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