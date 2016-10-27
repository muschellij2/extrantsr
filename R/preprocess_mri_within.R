#' @title Within Visit Registration
#'
#' @description This function performs skull stripping of the first image, 
#' within-visit registration using a rigid-body transformation, 
#' inhomogeneity correction, and potentially re-correcting after skull stripping.
#' @param files filenames (or nifti objects) of images to be processed.
#' Will register to the first scan
#' @param outfiles (character) name of output files, with extension
#' @param correct do Bias field correction
#' @param correction (character) N3 or N4 correction?
#' @param shrinkfactor Shrink factor passed to
#' \code{\link{n3BiasFieldCorrection}}
#' @param retimg (logical) return list of images of class nifti
#' @param reorient (logical) If retimg, should file be reoriented
#' when read in?
#' Passed to \code{\link{readnii}}.
#' @param typeofTransform type of transformed used, passed to
#' \code{\link{antsRegistration}}
#' @param interpolator Interpolation to be performed, passed to
#' \code{\link{antsRegistration}}
#' @param skull_strip do Skull stripping with FSL BET
#' @param bet.opts Options to pass to \code{\link{fslbet}}
#' @param betcmd Command to pass to \code{\link{fslbet}}
#' @param maskfile Filename (or nifti object) of mask for image to be
#' registered to
#' @param correct_after_mask Should the inhomogeneity correction be run
#' after masking.  If \code{correct = FALSE}, then 
#' @param verbose Diagnostic messages
#' @param ... arguments to \code{\link{bias_correct}} or
#' \code{\link{within_visit_registration}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @export
#' @return List of outfiles, maskfile, and output from \code{\link{registration}}.
preprocess_mri_within <- function(files,
                                  outfiles = NULL,
                                  correct = TRUE,  # do N3 Bias correction
                                  correction = "N3",
                                  shrinkfactor= "4",
                                  retimg = FALSE,
                                  reorient = FALSE,
                                  typeofTransform = "Rigid",
                                  interpolator = "LanczosWindowedSinc",
                                  skull_strip = FALSE,
                                  bet.opts = "-B -f 0.1 -v",
                                  betcmd = "bet",
                                  maskfile = NULL,
                                  correct_after_mask = FALSE,
                                  verbose = TRUE,
                                  ... # arguments to \code{\link{antsApplyTransforms}}
){
  
  # Expanding paths for ANTsR
  files = checkimg(files)
  
  ##################
  # Checking on outfiles or return images
  ##################
  if (retimg) {
    if (is.null(outfiles)) {
      outfiles = sapply(seq_along(files), function(x) {
        tempfile(fileext = ".nii.gz")
      })
    }
  } else {
    stopifnot(!is.null(outfiles))
  }
  
  outfiles = path.expand(outfiles)
  
  ##################
  # Must have extension
  ##################
  if (!all(grepl("[.]nii", c(files)))) {
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  ###################################
  # Skull stripping baseline T1 image
  ###################################
  if (skull_strip) {
    if (is.null(maskfile)) {
      brain_mask_stub = tempfile()
      fslext = suppressWarnings(get.imgext())
      maskfile = paste0(brain_mask_stub, fslext)
    } else {
      maskfile = checkimg(maskfile)
      # stopifnot(!file.exists(maskfile)) # making sure it doesn't exist
      # this check stops the option to have a maskfile and bet to be run 
      #  and make the file
    }
    if (!file.exists(maskfile)) {
      if (verbose) {
        message("Skull stripping files[1] image \n")
      }
      fslbet(infile = files[1],
             outfile = maskfile,
             retimg = FALSE,
             opts = bet.opts,
             betcmd = betcmd,
             verbose = verbose, reorient = FALSE)
      fslbin(file = maskfile, outfile = maskfile, retimg = FALSE,
             verbose = verbose)
    }
  }
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (correct) {
    if (!all(grepl("[.]nii", c(outfiles)))) {
      warning(paste0("Extensions not specified for ",
                     "outfiles, adding .nii.gz"))
      outfiles[!grepl("[.]nii",outfiles)] = paste0(
        outfiles[!grepl("[.]nii",outfiles)], '.nii.gz')
    }
  }
  
  stopifnot(length(outfiles) == length(files))
  
  stopifnot(all(file.exists(files)))
  #######################################
  # N3 Correction
  #######################################
  if (correct) {
    if (verbose) {
      message(paste0("# ", correction, " Correction"))
    }
    for (ifile in seq_along(files)) {
      bias_correct(file = files[ifile], outfile = outfiles[ifile],
                   retimg = FALSE,
                   correction = correction,
                   shrinkfactor = shrinkfactor, ...)
    }
    ### use the output files for processing
    files = outfiles
  }
  
  #######################################
  # Registration to first scan
  #######################################
  file1 = files[1]
  if (length(files) > 1) {
    other.files = files[seq(2, length(files), by = 1)]
    other.outfiles = outfiles[seq(2, length(files), by = 1)]
    
    regs = within_visit_registration(
      fixed = file1, # filename of T1 image
      moving = other.files,
      outfiles = other.outfiles,
      typeofTransform = typeofTransform,
      interpolator = interpolator,
      retimg = FALSE,
      ...)
  } else {
    regs = NULL
  }
  
  #######################################
  # Masking Brain
  #######################################
  if (!is.null(maskfile)) {
    maskfile = checkimg(maskfile)
    for (ifile in seq_along(outfiles)) {
      f = outfiles[ifile]
      fslmask(file = f, mask = maskfile,
              outfile = f,
              retimg = FALSE)
    }
  }

  #######################################
  # N3 Correction
  #######################################
  if (correct_after_mask) {
    if (!is.null(maskfile)) {
      warning(paste0(
        "correct_after_mask = TRUE, but no maskfile given!",
        " Not running bias_correct")
      )
    } else {
      if (verbose) {
        message(paste0("# ", correction, " Correction after Masking"))
      }
      for (ifile in seq_along(outfiles)) {
        bias_correct(
          file = outfiles[ifile], 
          outfile = outfiles[ifile],
          retimg = FALSE,
          correction = correction,
          shrinkfactor = shrinkfactor, 
          mask = maskfile,
          ...)
      }
    }
  }
  
  
  # Returning images
  if (retimg) {
    outfiles = lapply(outfiles, readnii, reorient = reorient)
  }
  L = c(outfiles = outfiles, maskfile = maskfile, regs = regs)
  rm(list = c("outfiles", "regs", "maskfile"))
  for (i in 1:10) {
    gc()
  }
  return(L)
}



