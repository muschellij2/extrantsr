#' @title Preprocess MRI acros visits
#'
#' @description This function performs preprocessing and registration
#' across visits and within visits
#' using ANTsR and SyN transformation
#' @param baseline_files filename of baseline images
#' @param followup_files filename of followup images
#' @param baseline_outfiles output filenames for baseline images
#' @param followup_outfiles output filenames for followup images
#' @param retimg 
#' @param maskfile 
#' @param skull_strip do Skull stripping with FSL BET
#' @param bet.opts 
#' @param betcmd 
#' @param within.transform Transformation for within-visit registration
#' @param within.interpolator Interpolator for within-visit registration
#' @param across.transform Transformation for across-visit registration
#' @param across.interpolator Interpolator for across-visit registration
#' @param verbose print diagnostic outputs
#' @param ... arguments to \code{\link{preprocess_mri_within}} and \code{\link{ants_regwrite}}
#' @export
#' @return NULL or list of objects of class nifti for transformed images
preprocess_mri_across <- function(baseline_files, # filename of baseline images
                  followup_files, # filename of followup images                
                  baseline_outfiles, # output filenames for baseline images
                  followup_outfiles, # output filenames for followup images
                  retimg = FALSE,
                  maskfile = NULL,
                  skull_strip = FALSE, # do Skull stripping with FSL BET
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
      fslext = suppressWarnings(get.imgext())
      brain_mask_stub = tempfile()
      maskfile = paste0(brain_mask_stub, fslext)
      if (verbose){
        cat("Skull stripping baseline[1] image \n")
      }
      fslbet(infile = baseline_outfiles[1], 
             outfile = brain_mask_stub, 
             retimg = FALSE,
             opts = bet.opts, 
             betcmd = betcmd, 
             verbose = verbose, reorient = FALSE)
    } else {
      maskfile = checkimg(maskfile)
    }
  } 
  if (verbose){
    cat("# Processing baseline data\n")
  }
  preprocess_mri_within(files = baseline_outfiles, 
                        outfiles = baseline_outfiles, 
                        reorient = FALSE, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator, 
                        maskfile = maskfile,
                        ...)
  #### Doing Followup
  preprocess_mri_within(files = followup_outfiles, 
                        outfiles = followup_outfiles, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator,                         
                        reorient = FALSE, ...) 
  
  #######################################
  ## Registering Followup to Baseline
  ####################################### 
  if (length(followup_outfiles) <= 1){
    other.files = NULL
    other.outfiles = NULL
  }
  ants_regwrite(filename = followup_outfiles[1], 
                outfile = followup_outfiles[1],
                template.file = baseline_outfiles[1], 
                typeofTransform = across.transform,
                interpolator = across.interpolator,
                other.files = other.files,
                other.outfiles = other.outfiles, 
                skull_strip = FALSE, 
                n3correct = FALSE, 
                retimg = FALSE,
                remove.warp = TRUE, ...)
  
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
    base = lapply(baseline_outfiles, readNIfTI, reorient = FALSE)
    fup = lapply(followup_outfiles, readNIfTI, reorient = FALSE)
    L = list(baseline=base, followup = fup)
    return(L)
  } 
  return(invisible(NULL))
}

#   ants_regwrite(filename = bout1, 
#                 outfile = bout1,
#                 template.file = template.file,
#                 typeofTransform = templateTransform,
#                 other.files = c(baseline_outfiles, fout1, followup_outfiles),
#                 other.outfiles = c(baseline_outfiles, fout1, followup_outfiles), 
#                 skull_strip = FALSE, 
#                 n3correct = FALSE, 
#                 retimg = FALSE,
#                 remove.warp = TRUE)   



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
  
  outfiles = path.expand(outfiles)
  
  if (!all(grepl("[.]nii", files))){
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (force_nii){
    if (!all(grepl("[.]nii", c(baseline_outfiles, followup_outfiles)))){
      warning(paste0("Extensions not specified for baseline_outfiles or ", 
                     "followup_outfiles, adding .nii.gz"))
      baseline_outfiles[!grepl("[.]nii",baseline_outfiles)] = paste0(
        baseline_outfiles[!grepl("[.]nii",baseline_outfiles)], '.nii.gz')
      followup_outfiles[!grepl("[.]nii",followup_outfiles)] = paste0(
        followup_outfiles[!grepl("[.]nii",followup_outfiles)], '.nii.gz')    
    }
  }
  
  stopifnot(file.exists(files))
  
  
  #######################################
  # Working on copy of Baseline 1, will be easier when n3 or ss
  #######################################
  nii_or_gz = function(x){
    ifelse(grepl("[.]nii[.]gz$", x), ".nii.gz", ".nii")
  }
  
  vec_extension = function(x, y, copyfiles){
    ext.base = nii_or_gz(x)
    if (ext.base != nii_or_gz(y)){
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
  if (n.in != n.out){
    stop("Number of input files must be number of outfiles")
  }  
  
  l = list(files = files, outfiles = outfiles)
  return(l)
}


