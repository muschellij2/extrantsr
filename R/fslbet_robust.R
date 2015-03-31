#' @title Robust Skull Stripping with COG estimation and Bias Correction
#' @description Skull Stripping (using FSL's BET) a file using \code{fslr}
#' functions and robustified by registration and neck removal
#' @param img (character) File to be skull stripped or object of class
#' nifti
#' @param outfile (character) output filename
#' @param retimg (logical) return image of class nifti
#' @param correct Perform bias field correction
#' @param correction (character) N3 or N4 correction?  
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param nvoxels Number of voxels to dilate/erode.  See \code{\link{dil_ero}}.
#' If \code{nvoxels = 0}, then no smoothing is done.
#' @param swapdim Use \code{\link{fslswapdim}} to reorient image
#' @param remove.neck Run \code{\link{remove_neck}} to register the template to a 
#' thresholded image to remove neck slices.
#' @param verbose (logical) Should diagnostic output be printed?
#' @param ... additional arguments passed to \code{\link{CT_Skull_Strip}} or 
#' \code{\link{remove_neck}}.
#' @return Skull-stripped \code{nifti} object 
#' @import fslr
#' @note This function first thresholds an image, runs a rigid registration
#' (default in \code{\link{remove_neck}}) to drop any slices below the transformed
#' skull stripped template to remove neck slices.  The neck-removed image is 
#' then skull stripped using defaults in \code{\link{fslbet}}.  A new 
#' center of gravity is estiamted using \code{\link{cog}}, then the image is
#' skull stripped again using the new cog. After the skull stripped mask is 
#' created, the image is dilated and eroded using \code{\link{dil_ero}} to
#' fill holes using a box kernel with the number of voxels \code{nvoxels} in 
#' all 3 directions.
#' @export
fslbet_robust <- function(
  img, 
  outfile = NULL,
  retimg = FALSE,
  correct = TRUE,
  correction = "N4",
  reorient = FALSE,  
  bet.opts = "",
  nvoxels = 5,
  swapdim = FALSE,
  remove.neck = TRUE,
  verbose=TRUE,
  ...
){
  
  outfile = check_outfile(outfile = outfile, retimg = retimg, fileext = "")
  outfile = nii.stub(outfile)
  
  #############################
  # Threshold Image
  #############################
  img = check_nifti(img, reorient = reorient)
  
  if (correct){
    if (verbose){
      cat("# Running Bias-Field Correction\n")
    }
    n4img = bias_correct(img, correction = correction, retimg = TRUE)
  } else {
    n4img = img
  }
  #############################
  # Removing Neck
  #############################
  if (remove.neck){
    forms = getForms(n4img)
    sorient = forms$ssor  
    if (swapdim){
      cat(paste0("# Swapping Dimensions \n"))
#       qorient = forms$sqor
      n4img = fslswapdim(file=n4img, retimg=TRUE, a="RL", b="PA", c="IS")
    } 
    if (verbose){
      cat(paste0("# Removing Neck\n"))
    }
    noneck = remove_neck(n4img, 
                         rep.value=0, ...)  
    if (swapdim){
      cat(paste0("# Swapping Dimensions Back\n"))
      noneck = fslswapdim(file=noneck, retimg=TRUE, a=sorient[1], 
                          b=sorient[2], c=sorient[3])
      n4img = fslswapdim(file=n4img, retimg=TRUE, a=sorient[1], 
                          b=sorient[2], c=sorient[3])      
    } 
  } else {
    noneck = n4img
  }
  
  #############################
  # Skull Stripping no-neck image
  #############################
  if (verbose){
    cat(paste0("# Skull Stripping for COG\n"))
  }
  brain1 = fslbet(noneck, retimg=TRUE, opts = bet.opts, verbose = verbose)
  if (verbose){
    cat(paste0("# Skull Stripping with new cog\n"))
  }
  xyz = ceiling(fslcog(brain1, mm = FALSE, verbose=FALSE))
  opts =  paste("-c", paste(xyz, collapse = " "))
  opts = paste(bet.opts, opts)  
  brain2 = fslbet(noneck, retimg= TRUE, 
                  opts = opts, verbose = verbose)
  ssmask = cal_img(brain2 > 0)
  
  #############################
  # Filling the mask
  ############################# 
  if (verbose){
    cat(paste0("# Filling Holes \n"))
  }
  if (nvoxels > 0){
    kopts = paste0("-kernel boxv ", nvoxels)
    ssmask = fslfill2(ssmask, 
                   retimg = TRUE, 
                   kopts = kopts,
                   remove.ends = FALSE,
                   refill = FALSE)
  }
  
  ss = mask_img(n4img, ssmask)
  
  ss = drop_img_dim(ss)
  writeNIfTI(ss, 
             filename = outfile)
  
  return(ss)
}