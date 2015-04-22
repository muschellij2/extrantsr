#' @title Multi-Atlas Label Fusion
#'
#' @description Takes in an input file and template images with 
#' a set of template structures and creates a label fusion
#' @param infile Input Image file
#' @param template.image Template Images to register 
#' to \code{infile}
#' @param template.structs Template gold standards to apply 
#' registration into \code{infile} space
#' @param keep_images Keep the \code{template.structs} in 
#' \code{infile} space
#' @param outfiles Output filenames for  \code{template.structs} in 
#' \code{infile} space
#' @param outfile Fused output filename
#' @param retimg Return Image to user using \code{\link{readNIfTI}}
#' @param ... Arguments to be passed to \code{\link{ants_regwrite}}
#' @export
#' @import matrixStats
#' @import fslr
#' @return The output filename or the nifti image
malf <- function(infile, template.image, template.structs,
                keep_images = TRUE, 
                outfiles = NULL,
                outfile = NULL, 
                retimg = TRUE,
                ...){
    
    nimgs = length(template.images)
    stopifnot(nimgs == length(template.structs))
    if (keep_images){
      stopifnot(length(outfiles) == nimgs)
    }
    if (is.null(outfiles)){
      outfiles = sapply(seq(nimgs), function(x){
        tempfile(fileext = ".nii.gz")
      })
    }
    outfile = check_outfile(outfile = outfile, retimg = retimg, 
                            fileext = "")
    
    if (verbose){
      cat("# Doing Registrations\n")
      pb = txtProgressBar(min=0, max=nimgs, style=3)     
    }    
    for (iimg in seq_along(template.images)){
      timage = template.images[[iimg]]
      tstruct = template.structs[[iimg]]
      ofile = outfiles[[iimg]]
      ants_regwrite(filename = timage, 
                    outfile = tempfile(fileext = ".nii.gz"),
                    retimg = FALSE,
                    template.file = infile,
                    other.files = tstruct,
                    other.outfiles = ofile,
                    ...)
      if (verbose){
        setTxtProgressBar(pb, iimg)
      }
    }
    if (verbose){
      close(pb)         
    }
    if (verbose){
      cat("# Reading in Files\n")
    }
    oimgs = lapply(outfiles, readNIfTI, reorient = FALSE)
    mat = do.call(oimgs, c)
    outimg = niftiarr(oimgs[[1]], rowMeans(mat))
    writeNIfTI(outimg, filename = outfile)
    if (retimg){
      return(outimg)
    } else {
      return(outfile)
    }
  
}