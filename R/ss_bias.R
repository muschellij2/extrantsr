#' @title Skull Strip then Bias Field Correct
#'
#' @description This function wraps \code{fslbet} and 
#' \code{bias_correct}, performing skull stripping first
#' @param filename filename to be processed
#' @param maskfile filename of masked to be passed in, otherwise bet is performed 
#' @param outfile output filename with extension (should be .nii.gz
#' @param skull_strip should skull stripping be done
#' @param bet.opts options for fslbet
#' @param betcmd bet command to be used see \code{\link{fslbet}}
#' @param n3correct should n3 correction be done stripping be done
#' @param correction correction method used see \code{\link{bias_correct}}
#' @param shrinkfactor correction method used see \code{\link{N3BiasFieldCorrection}}
#' @param verbose Diagnostic output
#' @param ... passed to \code{\link{bias_correct}}
#' @export
#' @seealso \code{\link{bias_ss}}
#' @return NULL
ss_bias <- function(filename, # filename to be processed
                    maskfile = NULL,
                    outfile = filename, # output filename with extension (should be .nii.gz)
                    skull_strip = TRUE, # should skull stripping be done
                    bet.opts = "-B -f 0.1 -v",
                    betcmd = "bet",                        
                  n3correct = TRUE, # should n3 correction be done stripping be done
                  correction = "N3", # correction method used, see \code{\link{bias_correct}}
                  shrinkfactor= "4", # correction method used, see \code{\link{N3BiasFieldCorrection}}
                  verbose = TRUE,
                  ... # passed to \code{\link{bias_correct}}
                  ){
  
  #### Run BET    
  if (skull_strip){
    if (verbose){
      cat("# Skull Stripping")
    }
    if ( is.null(maskfile) ){
      fslbet(infile = filename, 
             outfile = outfile, 
             opts = bet.opts, 
             betcmd = betcmd, 
             retimg= FALSE)
    } else {
      fslmask(file=filename, outfile = outfile, mask = mask)
    }
  }
  if (skull_strip){
    filename = outfile
  }
  if (n3correct){
    if (verbose){
      cat("# Bias Correction")
    }    
    bias_correct(filename, outfile = outfile, 
                 retimg=FALSE, 
                 correction = correction,
                 shrinkfactor=shrinkfactor, ...)
  }
  return(invisible(NULL))
}


#' @title Bias Field Correct then Skull Strip  
#'
#' @description This function wraps \code{fslbet} and 
#' \code{bias_correct}, performing bias correction first
#' @param filename filename to be processed
#' @param maskfile filename of masked to be passed in, otherwise bet is performed 
#' @param outfile output filename with extension (should be .nii.gz
#' @param skull_strip should skull stripping be done
#' @param bet.opts options for fslbet
#' @param betcmd bet command to be used see \code{\link{fslbet}}
#' @param n3correct should n3 correction be done stripping be done
#' @param correction correction method used see \code{\link{bias_correct}}
#' @param shrinkfactor correction method used see \code{\link{N3BiasFieldCorrection}}
#' @param verbose Diagnostic output
#' @param ... passed to \code{\link{bias_correct}}
#' @export
#' @seealso \code{\link{ss_bias}}
#' @return NULL
bias_ss <- function(filename, # filename to be processed
                    maskfile = NULL,
                    outfile = filename, # output filename with extension (should be .nii.gz)
                    skull_strip = TRUE, # should skull stripping be done
                    bet.opts = "-B -f 0.1 -v",
                    betcmd = "bet",                        
                    n3correct = TRUE, # should n3 correction be done stripping be done
                    correction = "N3", # correction method used, see \code{\link{bias_correct}}
                    shrinkfactor= "4", # correction method used, see \code{\link{N3BiasFieldCorrection}}
                    verbose = TRUE,
                    ... # passed to \code{\link{bias_correct}}
){
  if (n3correct){
    if (verbose){
      cat("# Bias Correction")
    }        
    bias_correct(filename, outfile = outfile, 
                 retimg=FALSE, 
                 correction = correction,
                 shrinkfactor=shrinkfactor, ...)
  }  
  if (n3correct){
    filename = outfile
  }  
  #### Run BET    
  if (skull_strip){
    if (verbose){
      cat("# Skull Stripping")
    }
    if ( is.null(maskfile) ){
      fslbet(infile = filename, 
             outfile = outfile, 
             opts = bet.opts, 
             betcmd = betcmd, 
             retimg= FALSE)
    } else {
      fslmask(file=filename, outfile = outfile, mask = mask)
    }
  }

  return(invisible(NULL))
}