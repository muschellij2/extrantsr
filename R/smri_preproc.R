#' @title Standard MRI Preprocessing
#'
#' @description This function performs robust skull stripping on the 
#' T1, then preprocessing within visit
#' @param files filenames (or nifti objects) of images to be processed.
#' Will register to the first scan
#' @param window Should \code{\link{robust_window}} before running?
#' @param outfiles (character) name of output files, with extension
#' @param maskfile Filename (or nifti object) of mask for image to be
#' registered to
#' @param correct do Bias field correction
#' @param correction (character) N3 or N4 correction?
#' @param correct_after_mask Should the inhomogeneity correction be run
#' after masking.   
#' @param verbose Diagnostic messages
#' @param ... arguments to \code{\link{preprocess_mri_within}} 
#' @export
#' @return List from \code{\link{preprocess_mri_within}} 
smri_preproc <- function(
  files,
  window = FALSE,
  outfiles = NULL,
  maskfile = NULL,
  correct = TRUE,
  correction = "N4",
  correct_after_mask = TRUE,
  verbose = TRUE,
  ... # arguments to \code{\link{antsApplyTransforms}}
){
  if (window) {
    files = check_nifti(files)
    files = robust_window(files)
  }
  
  files = checkimg(files)

  ss = fslbet_robust(
    files[1], 
    correct = TRUE,
    correction = "N4",
    swapdim = TRUE,
    nvoxels = 0,
    retimg = TRUE,
    outfile = NULL,
    remove.neck = TRUE,
    remover = "double_remove_neck",
    robust.mask = FALSE,
    bet.opts = ifelse(verbose, "-v", "")
  )
  mask = ss > 0
  if (is.null(maskfile)) {
    maskfile = tempfile(fileext = ".nii.gz")
  } 
  writenii(mask, filename = maskfile)
  
  if (is.null(outfiles)) {
    outfiles = sapply(seq_along(files),
                      function(x) {
                      tempfile(fileext = ".nii.gz")
                      })
  } else {
    outfiles = path.expand(outfiles)
  }
  
  pre = preprocess_mri_within(
    files = files,
    outfiles = outfiles,
    correct = correct,
    correction = correction,
    skull_strip = FALSE,
    maskfile = maskfile,
    correct_after_mask = correct_after_mask,
    verbose = verbose,
    ...)
  
  return(pre)
  
}