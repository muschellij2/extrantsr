#' @title Skull Strip then Bias Field Correct
#'
#' @description This function wraps \code{fslbet} and 
#' \code{bias_correct}, performing skull stripping first
#' @param filename filename to be processed
#' @param maskfile filename of masked to be passed in, otherwise bet is performed 
#' @param outfile output filename with extension (e.g. \code{.nii.gz})
#' @param skull_strip should skull stripping be done
#' @param bet.opts options for fslbet
#' @param betcmd bet command to be used see \code{\link{fslbet}}
#' @param correct should n3 correction be done stripping be done
#' @param correction correction method used see \code{\link{bias_correct}}
#' @param shrinkfactor correction method used see \code{\link{n3BiasFieldCorrection}}
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param verbose Diagnostic output
#' @param ... passed to \code{\link{bias_correct}}
#' @export
#' @seealso \code{\link{bias_ss}}
#' @return Filename of output file or object of class nifti
ss_bias <- function(filename, # filename to be processed
                    maskfile = NULL,
                    outfile = tempfile(fileext = ".nii.gz"), # output filename with extension (should be .nii.gz)
                    skull_strip = TRUE, # should skull stripping be done
                    bet.opts = "",
                    betcmd = "bet",                        
                    correct = TRUE, # should n3 correction be done stripping be done
                    correction = "N3", # correction method used, see \code{\link{bias_correct}}
                    shrinkfactor= "4", # correction method used, see \code{\link{n3BiasFieldCorrection}}
                    retimg = TRUE,
                    reorient = FALSE,
                    verbose = TRUE,
                    ... # passed to \code{\link{bias_correct}}
){
  
  #### Run BET    
  if (skull_strip) {
    if (verbose) {
      message("# Skull Stripping\n")
    }
    if ( is.null(maskfile) ) {
      fslbet(infile = filename, 
             outfile = outfile, 
             opts = bet.opts, 
             betcmd = betcmd, 
             retimg = FALSE,
             verbose = verbose)
    } else {
      fslmask(file = filename, outfile = outfile, mask = maskfile,
              verbose = verbose)
    }
  }
  if (skull_strip) {
    filename = outfile
  }
  if (correct) {
    if (verbose) {
      message("# Bias Correction\n")
    }    
    bias_correct(filename, outfile = outfile, 
                 retimg = FALSE, 
                 correction = correction,
                 shrinkfactor = shrinkfactor, ...)
  }
  ####### 
  # Returning image
  ###########
  if (retimg) {
    img = readNIfTI(outfile, reorient = reorient)
    return(img)
  } else {
    return(outfile)
  }
  
  # return(invisible(NULL))
}


#' @title Bias Field Correct then Skull Strip  
#'
#' @description This function wraps \code{fslbet} and 
#' \code{bias_correct}, performing bias correction first
#' @param filename filename to be processed
#' @param maskfile filename of masked to be passed in, otherwise bet is performed 
#' @param outfile output filename with extension (e.g. \code{.nii.gz})s
#' @param skull_strip should skull stripping be done
#' @param bet.opts options for fslbet
#' @param betcmd bet command to be used see \code{\link{fslbet}}
#' @param correct should n3 correction be done stripping be done
#' @param correction correction method used see \code{\link{bias_correct}}
#' @param shrinkfactor correction method used see \code{\link{n3BiasFieldCorrection}}
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}.  
#' @param verbose Diagnostic output
#' @param ... passed to \code{\link{bias_correct}}
#' @export
#' @seealso \code{\link{ss_bias}}
#' @return Filename of output file or object of class nifti
bias_ss <- function(filename, # filename to be processed
                    maskfile = NULL,
                    outfile = tempfile(fileext = ".nii.gz"), # output filename with extension (should be .nii.gz)
                    skull_strip = TRUE, # should skull stripping be done
                    bet.opts = "",
                    betcmd = "bet",                        
                    correct = TRUE, # should n3 correction be done stripping be done
                    correction = "N3", # correction method used, see \code{\link{bias_correct}}
                    shrinkfactor= "4", # correction method used, see \code{\link{n3BiasFieldCorrection}}
                    retimg = TRUE,
                    reorient = FALSE,                    
                    verbose = TRUE,
                    ... # passed to \code{\link{bias_correct}}
){
  if (correct) {
    if (verbose) {
      message("# Bias Correction\n")
    }        
    bias_correct(filename, outfile = outfile, 
                 retimg = FALSE, 
                 correction = correction,
                 shrinkfactor = shrinkfactor, ...)
  }  
  if (correct) {
    filename = outfile
  }  
  #### Run BET    
  if (skull_strip) {
    if (verbose) {
      message("# Skull Stripping\n")
    }
    if ( is.null(maskfile) ) {
      fslbet(infile = filename, 
             outfile = outfile, 
             opts = bet.opts, 
             betcmd = betcmd, 
             retimg = FALSE,
             verbose = verbose)
    } else {
      fslmask(file = filename, outfile = outfile, mask = maskfile,
              verbose = verbose)
    }
  }
  ####### 
  # Returning image
  ###########
  if (retimg) {
    img = readNIfTI(outfile, reorient = reorient)
    return(img)
  } else {
    return(outfile)
  }
  
  return(invisible(NULL))
}