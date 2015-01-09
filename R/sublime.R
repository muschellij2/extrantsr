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
sublime <- function(baseline_files, # filename of T1 image
                  followup_files, # do Skull stripping with FSL BET
                  baseline_outfiles, 
                  followup_outfiles,
                  typeofTransform = "Rigid",
                  skull_strip = FALSE, # do Skull stripping with FSL BET
                  bet.opts = "-B -f 0.1 -v",
                  betcmd = "bet",                                    
                  n3correct = FALSE,  # do N3 Bias correction
                  correction = "N3", 
                  shrinkfactor= "4",               
                  template.file = file.path(fsldir(),
                                            "data", "standard", "MNI152_T1_1mm_brain.nii.gz"),
                  templateTransform = "Affine",
                  verbose = TRUE,
                  ... # arguments to \code{\link{antsApplyTransforms}} 
){
  # Expanding paths for ANTsR
  baseline_files = path.expand(baseline_files)
  baseline_outfiles = path.expand(baseline_outfiles)
  
  followup_files = path.expand(followup_files)
  followup_outfiles = path.expand(followup_outfiles)
  
  if (!all(grepl("[.]nii", c(baseline_files, followup_files)))){
    stop("All filenames must be nifti .nii or .nii.gz")
  }
  
  #############################
  # Checking if extensions are .nii or .nii.gz
  #############################
  if (n3correct){
    if (!all(grepl("[.]nii", c(baseline_outfiles, followup_outfiles)))){
      warning(paste0("Extensions not specified for baseline_outfiles or ", 
                     "followup_outfiles, adding .nii.gz"))
      baseline_outfiles[!grepl("[.]nii",baseline_outfiles)] = paste0(
        baseline_outfiles[!grepl("[.]nii",baseline_outfiles)], '.nii.gz')
      followup_outfiles[!grepl("[.]nii",followup_outfiles)] = paste0(
        followup_outfiles[!grepl("[.]nii",followup_outfiles)], '.nii.gz')    
    }
  }
  
  stopifnot(file.exists(baseline_files))
  stopifnot(file.exists(followup_files))
  
  base1 = baseline_files[[1]]
  bout1 = baseline_outfiles[[1]]
  fup1 = followup_files[[1]]
  fout1 = followup_outfiles[[1]]
  
  #######################################
  # Working on copy of Baseline 1, will be easier when n3 or ss
  #######################################
  nii_or_gz = function(x){
    ifelse(grepl("[.]nii[.]gz$"), ".nii.gz", ".nii")
  }
  
  ext.base = nii_or_gz(base1)
  if (ext.base != nii_or_gz(bout1)){
    warning("Extension of Baseline[1] is not same as baseline out[1], forcing")
    bout1 = paste0(nii.stub(bout1), ext.base)
  }
  file.copy(from = base1, to = bout1)
  stopifnot(file.exists(bout1))
  
  
  # Remove first scan
  followup_files = followup_files[[-1]]
  baseline_files = baseline_files[[-1]]
  
  followup_outfiles = followup_outfiles[[-1]]
  baseline_outfiles = baseline_outfiles[[-1]]
  
  
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


  func = extrants::ss_bias
  #######################################
  ## Creating wrapper function for worker
  ####################################### 
  wrapper = function(file1, files, 
                     bet.opts = bet.opts, betcmd= betcmd, 
                     skull_strip=skull_strip, 
                     n3correct = n3correct, 
                     correction = correction, 
                     shrinkfactor= shrinkfactor,
                     verbose = verbose,
                     ...){
    if (length(files) > 0){
      if (verbose){
        cat(paste0("# Running ", printer, " Data"))
      }
      if (skull_strip | n3correct){
        func(file1, bet.opts = bet.opts, betcmd= betcmd, 
               skull_strip=skull_strip, 
               n3correct = n3correct, 
               correction = correction, 
               shrinkfactor= shrinkfactor, 
             verbose = verbose)
        
        sapply(files, function(filename){
          func(filename, bet.opts= bet.opts, betcmd= betcmd, 
                 skull_strip=skull_strip, 
                 n3correct = n3correct, 
                 correction = correction, 
                 shrinkfactor= shrinkfactor, 
               verbose = verbose)      
        })
      }
      
      within_visit_registration(file1, # filename of T1 image
                                files,
                                files, 
                                typeofTransform = typeofTransform,
                                retimg = FALSE, 
                                ...)
    }
  }
  
  #######################################
  ## Followup images to followup1
  #######################################    
  wrapper(bout1, baseline_outfiles, 
          verbose = verbose,
          printer = "Baseline",
          bet.opts = bet.opts, betcmd= betcmd, 
          skull_strip = skull_strip, 
          n3correct = n3correct, 
          correction = correction, 
          shrinkfactor= shrinkfactor, ... = ...)
  
  wrapper(fout1, followup_outfiles, 
          verbose = verbose,
          printer = "Followup",
          bet.opts = bet.opts, betcmd= betcmd, 
          skull_strip = skull_strip, 
          n3correct = n3correct, 
          correction = correction, 
          shrinkfactor= shrinkfactor, ... = ...)  
  

  #######################################
  ## Registering Followup to Baseline
  #######################################  
  ants_regwrite(filename = fout1, 
                outfile = fout1,
                template.file = bout1, 
                typeofTransform = typeofTransform,
                other.files = followup_outfiles,
                other.outfiles = followup_outfiles, 
                skull_strip = FALSE, 
                n3correct = FALSE, 
                retimg = FALSE)
  
  #######################################
  ## Registering Baseline to Template
  #######################################   
  ants_regwrite(filename = bout1, 
                outfile = bout1,
                template.file = template.file,
                typeofTransform = templateTransform,
                other.files = c(baseline_outfiles, followup_outfiles),
                other.outfiles = c(baseline_outfiles, followup_outfiles), 
                skull_strip = FALSE, 
                n3correct = FALSE, 
                retimg = FALSE) 
  
  ##### Need to implement normalization
  
  return(invisible(NULL))
}



