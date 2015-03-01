#' @title OASIS Processing Pipeline
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param files filenames (or nifti objects) of images to be processed.
#' Will register to the first scan
#' @param outfiles (character) name of output files, with extension
#' @param n3correct do N3 Bias correction
#' @param correction (character) N3 or N4 correction?
#' @param shrinkfactor Shrink factor passed to 
#' \code{\link{n3BiasFieldCorrection}} 
#' @param retimg (logical) return list of images of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}} 
#' @param interpolator Interpolation to be performed, passed to
#' \code{\link{antsRegistration}} 
#' @param skull_strip do Skull stripping with FSL BET
#' @param bet.opts Options to pass to \code{\link{fslbet}}
#' @param betcmd Command to pass to \code{\link{fslbet}}
#' @param maskfile Filename (or nifti object) of mask for image to be
#' registered to
#' @param verbose Diagnostic messages
#' @param ... arguments to \code{\link{bias_correct}} or 
#' \code{\link{within_visit_registration}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @export
#' @return NULL or object of class nifti for transformed T1 image
preprocess_mri_within <- function(files, 
                              outfiles = NULL,
                              n3correct = TRUE,  # do N3 Bias correction
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
                             verbose = TRUE,
                           ... # arguments to \code{\link{antsApplyTransforms}} 
){
  
  # Expanding paths for ANTsR
  files = checkimg(files)

  ##################
  # Checking on outfiles or return images
  ##################  
  if (retimg){
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
  if (!all(grepl("[.]nii", c(files)))){
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  ###################################
  # Skull stripping baseline T1 image
  ###################################
  if (skull_strip) {
    if (is.null(maskfile)){
      brain_mask_stub = tempfile()
      fslext = suppressWarnings(get.imgext())      
      maskfile = paste0(brain_mask_stub, fslext)
    } else {
      maskfile = checkimg(maskfile)
      stopifnot(file.exists(maskfile))
    }
    if (!file.exists(maskfile)){
      if (verbose){
        cat("Skull stripping files[1] image \n")
      }
      fslbet(infile = files[1], 
             outfile = maskfile, 
             retimg = FALSE,
             opts = bet.opts, 
             betcmd = betcmd, 
             verbose = verbose, reorient = FALSE)
      fslbin(file=maskfile, outfile = maskfile, retimg=FALSE,
             verbose = verbose)
    }
  }  
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (n3correct){
    if (!all(grepl("[.]nii", c(outfiles)))){
      warning("Extensions not specified for outfiles, adding .nii.gz")
      outfiles[!grepl("[.]nii",outfiles)] = paste0(
        outfiles[!grepl("[.]nii",outfiles)], '.nii.gz')  
    }
  }
  
  stopifnot(length(outfiles) == length(files))
  
  stopifnot(all(file.exists(files)))
  #######################################
  # N3 Correction
  #######################################
  if (n3correct){
    if (verbose){
      cat("# N3 Correction")
    }
    for (ifile in seq_along(files)){
      bias_correct(file = files[ifile], outfile = outfiles[ifile], 
               retimg=FALSE, 
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
  if (length(files) > 1){
    other.files = files[seq(2, length(files), by = 1)]
    other.outfiles = outfiles[seq(2, length(files), by = 1)]

    within_visit_registration(fixed=file1, # filename of T1 image
                            moving = other.files,
                            outfiles = other.outfiles, 
                            typeofTransform = typeofTransform,
                            interpolator = interpolator,
                            retimg = FALSE, 
                            ...)
  }

  #######################################
  # Masking Brain
  #######################################
  if (!is.null(maskfile)){
    maskfile = checkimg(maskfile)
    for (ifile in seq_along(outfiles)){
      f = outfiles[ifile]
      fslmask(file = f, mask = maskfile, 
              outfile = f, 
              retimg=FALSE)
    } 
  }

  
  # Returning images
  if (retimg){
    L = lapply(outfiles, readNIfTI, reorient = reorient)
    return(L)
  }
  return(outfiles)
}



