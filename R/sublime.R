#' @title OASIS Processing Pipeline
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param filename filename of T1 image
#' @param skull_strip do skull stripping with FSL BET 
#' @param skull_stripfile Output skull strip filename
#' @param n3correct do N3 Bias correction
#' @param normalize Normalize data using \code{\link{whitestripe}}
#' @param normalize_file \code{\link{whitestripe}} image mask
#' @param retimg return a nifti object from function
#' @param outfile output filename should have .nii or .nii.gz 
#' extension
#' @param template.file Filename of template to warp to
#' @param interpolator interpolation done for 
#' \code{\link{antsApplyTransforms}}
#' @param other.files Filenames of other iamges to be 
#' transformed with the T1
#' @param other.outfiles Output filenames of \code{other.files} to 
#' be written
#' @param typeofTransform type of transformed used, passed to 
#' \code{\link{antsRegistration}}
#' @param remove.warp (logical) Should warping images be deleted?
#' @param outprefix Character path of where the warp files should be stored.
#' Required if \code{remove.warp = FALSE}
#' @param bet.opts Options passed to \code{\link{fslbet}}
#' @param betcmd BET command used, passed to \code{\link{fslbet}}
#' @param ... arguments to \code{\link{whitestripe}}
#' @import ANTsR
#' @import fslr
#' @import oro.nifti
#' @import WhiteStripe
#' @export
#' @return NULL or object of class nifti for transformed T1 image
preprocess_mri_across <- function(baseline_files, # filename of T1 image
                  followup_files, # do Skull stripping with FSL BET                  
                  baseline_outfiles, 
                  followup_outfiles,
                  retimg = FALSE,
                  reorient = FALSE,                              
                  brain_mask = NULL,
                  skull_strip = FALSE, # do Skull stripping with FSL BET
                  bet.opts = "-B -f 0.1 -v",
                  betcmd = "bet",
                  within.transform = "Rigid",
                  within.interpolator =  "LanczosWindowedSinc",
                  across.transform = within.transform,
                  across.interpolator = within.interpolator,
                  template.file = file.path(fsldir(),
                                            "data", "standard", 
                                            ifelse(skull_strip, 
                                                   "MNI152_T1_1mm_brain.nii.gz",
                                                   "MNI152_T1_1mm.nii.gz")),
                  templateTransform = "Affine",
                  templateInerpolator = "Linear",
                  verbose = TRUE,
                  ... # arguments to \code{\link{antsApplyTransforms}} 
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
    
  #######################################
  # Checking filename lengths
  #######################################
  n.base = length(baseline_outfiles)
  n.baseout = length(baseline_outfiles)
  if (n.baseout != n.base){
    stop("Number of baseline files must be number of baseline outfiles")
  }

  n.fup = length(followup_files)
  n.fout = length(followup_outfiles)
  if (n.baseout != n.fup){
    stop("Number of followup files must be number of followup outfiles")
  }

  ###################################
  # Skull stripping baseline T1 image
  ###################################
  if (is.null(brain_mask)){
    if (skull_strip) {
      fslext = suppressWarnings(get.imgext())
      brain_mask_stub = tempfile()
      brain_mask = paste0(brain_mask_stub, fslext)
      if (verbose){
        cat("Skull stripping baseline[1] image \n")
      }
      fslbet(infile = baseline_outfiles[1], 
             outfile = brain_mask_stub, 
             retimg = FALSE,
             opts = bet.opts, 
             betcmd = betcmd, 
             verbose = verbose, reorient = reorient)
    }
  } 
  
  preprocess_mri_within(files = baseline_outfiles, 
                        outfiles = baseline_outfiles, 
                        reorient = reorient, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator, 
                        maskfile = brain_mask,
                        ...)
  #### Doing Followup
  preprocess_mri_within(files = followup_outfiles, 
                        outfiles = followup_outfiles, 
                        typeofTransform = within.transform,
                        interpolator = within.interpolator,                         
                        reorient = reorient, ...) 
  
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
                remove.warp = TRUE)
  

  
  #######################################
  ## Registering Baseline to Template
  #######################################  
  allfiles = c(baseline_outfiles, fout1, followup_outfiles)
  tfiles = sapply(allfiles, tempfile(fileext = ".nii.gz"))
  bout1.temp = tempfile(fileext = ".nii.gz")
  ants_regwrite(filename = bout1, 
                outfile = bout1.temp,
                template.file = template.file,
                typeofTransform = templateTransform,
                other.files = c(baseline_outfiles, fout1, followup_outfiles),
                other.outfiles = tfiles, 
                skull_strip = FALSE, 
                n3correct = FALSE, 
                retimg = FALSE,
                remove.warp = TRUE) 
  
#   img = readNIfTI(bout1.temp, reorient = FALSE)
#   ws = whitestripe(img = img, type="T1", verbose = verbose)
#   maskfile = tempfile()
#   writeNIfTI(mask.img, filename = maskfile)
    
  
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
  
  ##### Need to implement normalization
  
  return(invisible(NULL))
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
  
  outfiles = path.expand(outfiles)
  
  if (!all(grepl("[.]nii", c(files))){
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (force_nifti){
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
  mapply(vec_extension, 
         files, 
         outfiles, 
         copyfiles = copyfiles)
  
  l = list(files = files, outfiles = outfiles)
  return(l)
}