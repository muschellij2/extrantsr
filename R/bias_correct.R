#' @title N3 or N4 Correct
#' @description This function wraps ANTsR bias field corrections and returns
#' nifti objects
#' @param file (character) image to be manipulated
#' @param correction (character) N3 or N4 correction?
#' @param outfile (character) resultant image name (optional)
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readnii}}.
#' @param shrinkfactor Shrink factor passed to 
#' \code{\link{n3BiasFieldCorrection}}
#' @param mask Mask to pass to \code{\link{n4BiasFieldCorrection}} 
#' @param verbose print diagnostic output.
#' @param ... additional arguments passed to 
#' \code{\link{n3BiasFieldCorrection}} or 
#'  \code{\link{n4BiasFieldCorrection}} 
#' @return If \code{retimg} then object of class nifti.  Otherwise,
#' Result from system command, depends if intern is TRUE or FALSE.
#' @export
bias_correct = function(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  outfile = NULL, 
  retimg = TRUE,
  reorient = FALSE,
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...){
  
  outfile = neurobase::check_outfile(
    outfile = outfile, 
    retimg = retimg, 
    fileext = ".nii.gz")
  
  res = bias_correct_ants(
    file = file,
    correction = correction,
    shrinkfactor = shrinkfactor,
    mask = mask,
    verbose = verbose,
    ...)
  
  antsImageWrite(res, filename = outfile)
  rm(list = c("res")); 
  for (i in 1:10) {
    gc()
  }  
  
  if (retimg) {
    x = neurobase::readnii(outfile, reorient = reorient)
  } else {
    x = outfile
  }
  
  return(x)
}

#' @rdname bias_correct
#' @export
bias_correct_ants = function(
  file,
  correction = c("N3", "N4", "n3", "n4"),
  shrinkfactor = "4",
  mask = NULL,
  verbose = TRUE,
  ...){
  correction = toupper(correction)
  correction = match.arg(correction, c("N3", "N4"))
  
  img = check_ants(file)
  
  if (correction %in% c("N3", "n3")) {
    res = n3BiasFieldCorrection(
      img = img, 
      downsampleFactor = shrinkfactor, 
      ...)
  }
  if (correction %in% c("N4", "n4")) {
    if (!is.null(mask)) {
      mask = check_ants(x = mask)
    } else {
      # NULL/NA problem
      xxx = formals(n4BiasFieldCorrection)
      if (!is.name(xxx$mask)) {
        mask = xxx$mask
      }
    }
    
    args = list(...)
    # make it so that convergence has the correct list of params
    convergence = args$convergence
    if (!is.null(convergence)) {
      if (!is.list(convergence)) {
        stop("convergence must be a list")
      }
      form_n4 = formals(n4BiasFieldCorrection)
      check_names = setdiff(names(form_n4$convergence), "")
      
      n_conv = names(convergence)
      names_in = check_names %in% n_conv
      if (!all(names_in)) {
        warning(
          paste0(
            "Not all parameters in convergence that ", 
            "are needed, adding defaults"))
        use_def = check_names[ !names_in ]
        for (idef in use_def) {
          convergence[[idef]] = form_n4$convergence[[idef]]
        }
      }
    }
    args$img = img
    args$mask = mask
    args$shrinkFactor = as.numeric(shrinkfactor)
    args$verbose = verbose
    args$convergence = convergence
    
    res = do.call(n4BiasFieldCorrection, args = args)
    # img = img, 
    # mask = mask, 
    # shrinkFactor = as.numeric(shrinkfactor), 
    # verbose = verbose,
    # ...)
  }
  rm(list = c("img")); 
  for (i in 1:10) {
    gc()
  }  
  return(res)
}
