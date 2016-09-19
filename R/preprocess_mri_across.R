#' @title Preprocess MRI acros visits
#'
#' @description This function performs preprocessing and registration
#' across visits and within visits
#' @param baseline_files filename of baseline images
#' @param followup_files filename of followup images
#' @param baseline_outfiles output filenames for baseline images
#' @param followup_outfiles output filenames for followup images
#' @param retimg (logical) return list of images of class nifti
#' @param maskfile Filename (or nifti object) of binary mask for baseline image.
#' If filename exists, skull stripping will be done by masking it.
#' If it does not, then skull stripping will get one from the baseline
#' image
#' @param correct Perform bias field correction
#' @param correction (character) N3 or N4 correction? 
#' @param skull_strip do Skull stripping with FSL BET
#' @param bet.opts Options to pass to \code{\link{fslbet}}
#' @param betcmd Command to pass to \code{\link{fslbet}}
#' @param within.transform Transformation for within-visit registration
#' @param within.interpolator Interpolator for within-visit registration
#' @param across.transform Transformation for across-visit registration
#' @param across.interpolator Interpolator for across-visit registration
#' @param verbose print diagnostic outputs
#' @param ... arguments to \code{\link{preprocess_mri_within}} and \code{\link{ants_regwrite}}
#' @export
#' @return list of objects from \code{\link{preprocess_mri_within}} and
#' \code{\link{registration}} 
preprocess_mri_across <- function(baseline_files, # filename of baseline images
                  followup_files, # filename of followup images                
                  baseline_outfiles, # output filenames for baseline images
                  followup_outfiles, # output filenames for followup images
                  retimg = FALSE,
                  maskfile = NULL,
                  correct = TRUE,
                  correction = "N3",
                  skull_strip = TRUE, # do Skull stripping with FSL BET
                  bet.opts = "-B -f 0.1 -v",
                  betcmd = "bet",
                  within.transform = "Rigid", # Transformation for within-visit registration
                  within.interpolator =  "LanczosWindowedSinc", # Interpolator for within-visit registration
                  across.transform = within.transform, # Transformation for across-visit registration
                  across.interpolator = within.interpolator, # Interpolator for across-visit registration
                  verbose = TRUE, # print diagnostic outputs
                  ... # arguments to \code{\link{preprocess_mri_within}} and \code{\link{ants_regwrite}}  
){
  #################################
  # Making sure these are character filenames
  #################################
  bf = process_filenames(files = baseline_files, 
                         outfiles = baseline_outfiles, 
                         force_nii = TRUE,
                         copyfiles = TRUE)
  baseline_files = bf$files
  baseline_outfiles = bf$outfiles
  
  bf = process_filenames(files = followup_files, 
                         outfiles = followup_outfiles, 
                         force_nii = TRUE,
                         copyfiles = TRUE)
  followup_files = bf$files
  followup_outfiles = bf$outfiles  
    
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
        cat("Skull stripping baseline[1] image \n")
      }
      fslbet(infile = baseline_outfiles[1], 
             outfile = maskfile, 
             retimg = FALSE,
             opts = bet.opts, 
             betcmd = betcmd, 
             verbose = verbose, reorient = FALSE)
      fslbin(file=maskfile, outfile = maskfile, retimg=FALSE,
             verbose = verbose)
    }
  }
  
  ###################################
  # Processing basleine data
  ###################################  
  if (verbose){
    cat("# Processing baseline data\n")
  }
  proc_1 = preprocess_mri_within(files = baseline_outfiles, 
                        outfiles = baseline_outfiles, 
                        reorient = FALSE, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator, 
                        maskfile = maskfile,
                        correct = correct,
                        correction = correction,
                        verbose = verbose,
                        ...)
  #### Doing Followup
  proc_2 = preprocess_mri_within(files = followup_outfiles, 
                        outfiles = followup_outfiles, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator,                         
                        reorient = FALSE, 
                        correct = correct,
                        correction = correction,                        
                        verbose = verbose,                        
                        ...) 
  
  # get the transformations out
  proc2_trans = lapply(proc_2$regs, `[[`, "fwdtransforms")
  
  #######################################
  ## Registering Followup to Baseline
  ####################################### 
  if (length(followup_outfiles) <= 1) {
    other.files = NULL
    other.outfiles = NULL
  } else {
    other.outfiles = other.files = followup_outfiles[-1]
  }
  regs = registration(filename = followup_outfiles[1], 
                outfile = followup_outfiles[1],
                template.file = baseline_outfiles[1], 
                typeofTransform = across.transform,
                interpolator = across.interpolator,
                other.files = other.files,
                other.outfiles = other.outfiles, 
                other.init = proc2_trans,
                skull_strip = FALSE, 
                correct = FALSE, 
                retimg = FALSE,
                remove.warp = TRUE, 
                verbose = verbose, ...)
  
  #######################################
  # Masking Brain
  #######################################
  if (skull_strip){
    for (ifile in seq_along(followup_outfiles)){
      f = followup_outfiles[ifile]
      fslmask(file = f, mask = maskfile, 
              outfile = f, 
              retimg=FALSE, verbose = verbose)
    } 
  }  
  
  if (retimg){
    base = lapply(baseline_outfiles, readnii, reorient = FALSE)
    fup = lapply(followup_outfiles, readnii, reorient = FALSE)
  } else {
    base = fup = NULL
  }
  L = list(baseline = base, followup = fup)  
  L = c(L, regs = regs, proc_1 = proc_1, proc_2 = proc_2)
  return(L)
}




#' @title Process filenames to ensure nifti and consistency
#'
#' @description Takes input and output filename, ensures the same
#' file extension, copies files, and checks all are character
#' @param files input filenames
#' @param outfiles output filenames
#' @param force_nii Force output filenames to have .nii
#' @param copyfiles copy files to outfiles
#' @export
#' @return List of 2 elements: files of the filenames and outfiles
#' of output filenames
process_filenames <- function(files, # input filenames 
                             outfiles, # output filenames
                             force_nii = TRUE, # Force output filenames to have .nii
                             copyfiles = TRUE # copy files to outfiles
                             ){
  files = as.list(files)
  
  # Expanding paths for ANTsR - checkimg does expansion  
  files = sapply(files, checkimg)
  gc();
  
  outfiles = path.expand(outfiles)
  
  if (!all(grepl("[.]nii", files))) {
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (force_nii) {
    if (!all(grepl("[.]nii", c(outfiles)))) {
      warning(paste0("Extensions not specified for outfiles, adding .nii.gz"))
      outfiles[!grepl("[.]nii",outfiles)] = paste0(
        outfiles[!grepl("[.]nii",outfiles)], '.nii.gz')    
    }
  }
  
  stopifnot(file.exists(files))
  
  
  #######################################
  # Working on copy of Baseline 1, will be easier when n3 or ss
  #######################################
  nii_or_gz = function(x) {
    ifelse(grepl("[.]nii[.]gz$", x), ".nii.gz", ".nii")
  }
  
  vec_extension = function(x, y, copyfiles){
    ext.base = nii_or_gz(x)
    if (ext.base != nii_or_gz(y)) {
      warning(paste0("Extension of ", basename(x), "is not same as ", 
                     basename(y), ", forcing"))
      y = paste0(nii.stub(y), ext.base)
    }
    if (copyfiles) file.copy(from = x, to = y, overwrite = TRUE)
    return(y)
  }
  ##########################
  # Copying files over, now work on baseline_outfiles for output
  ##########################  
  outfiles = mapply(vec_extension, 
         files, 
         outfiles, 
         copyfiles = copyfiles)
  
  #######################################
  # Checking filename lengths
  #######################################  
  n.in = length(files)
  n.out = length(outfiles)
  if (n.in != n.out) {
    stop("Number of input files must be number of outfiles")
  }  
  
  l = list(files = files, outfiles = outfiles)
  return(l)
}


