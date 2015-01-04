#' @title Robust CT Skull Stripping 
#' @description Skull Stripping (using FSL's BET) a CT file using \code{fslr}
#' functions and robustified by registration
#' @param img (character) File to be skull stripped or object of class
#' nifti
#' @param outfile (character) output filename
#' @param keepmask (logical) Should we keep the mask?
#' @param maskfile (character) Filename for mask (if \code{keepmask = TRUE}).
#' If \code{NULL}, then will do \code{paste0(outfile, "_Mask")}.
#' @param retimg (logical) return image of class nifti
#' @param reorient (logical) If retimg, should file be reoriented when read in?
#' Passed to \code{\link{readNIfTI}}. 
#' @param lthresh (default: 0) Lower value to threshold CT 
#' \code{\link{fslthresh}}
#' @param uthresh (default: 100) Upper value to threshold CT
#' \code{\link{fslthresh}}
#' @param nvoxels Number of voxels to dilate/erode.  See \code{\link{dil_ero}}.
#' @param smooth.factor Smoothing factor for \code{\link{fslbet}}.  See \code{-w}
#' option in \code{fslbet.help()}.
#' @param verbose (logical) Should diagnostic output be printed?
#' @param opts Not used
#' @param ... additional arguments passed to \code{\link{CT_Skull_Strip}}.
#' @return Skull-stripped \code{nifti} object 
#' @import fslr
#' @importFrom cttools CT_Skull_Strip
#' @importFrom cttools dil_ero
#' @export
CT_Skull_Strip_robust <- function(
  img, 
  outfile = NULL,
  keepmask = TRUE,
  maskfile = NULL,
  retimg = FALSE,
  reorient = FALSE,                 
  int = "0.01", 
  lthresh = 0,
  uthresh = 100,  
  nvoxels = 5,
  smooth.factor = 2,
  verbose=TRUE,
  opts = NULL,
  ...
){

  if (is.null(outfile)) {
    outfile = tempfile()
  }  

  if (verbose){
    cat(paste0("# Thresholding Image to ", lthresh, "-", uthresh, "\n"))
  }
  
  outfile = nii.stub(outfile)
  
  ### Working on maskfile
  if (is.null(maskfile)){
    maskfile = nii.stub(outfile)
    maskfile = paste0(outfile, "_Mask")
  }  
  maskfile = nii.stub(maskfile)
  stopifnot(inherits(maskfile, "character"))
  
  #############################
  # Threshold Image
  #############################
  img = check_nifti(img, reorient = reorient)
  thresh = niftiarr(img, img * (img > lthresh & img < uthresh))
  ## need to do rep.value = 0 because template is like that.
  
  #############################
  # Removing Neck
  #############################
  neck_mask = remove_neck(thresh, 
                          rep.value=0, 
                          template.file = 
                            system.file("scct_unsmooth_Skull_Stripped.nii.gz", 
                                        package = "cttools"),
                          ret_mask = TRUE)
  
  noneck = mask_img(img, neck_mask)
  
  #############################
  # Skull Stripping no-neck image
  #############################  
  ss = CT_Skull_Strip(noneck, outfile = outfile, retimg = TRUE, 
                      maskfile = maskfile, 
                      opts = paste("-f ", int, ifelse(verbose, " -v", "")),
                      keepmask = TRUE, reorient = reorient, ...)
  
  ssmask = readNIfTI(maskfile, 
                     reorient = reorient)
  
  #############################
  # Setting new center of gravity and rerunning with smoothness factor
  #############################    
  cog = cog(ssmask, 
            ceil=TRUE)
  ss = CT_Skull_Strip(noneck, outfile = outfile, retimg = TRUE, 
                      opts = paste("-f ", int, ifelse(verbose, "-v", ""), 
                                   "-w ", smooth.factor, 
                                   paste(c("-c", cog), collapse=" ")),
                      maskfile = maskfile,
                      keepmask = TRUE, 
                      reorient = reorient,
                      ...)
  
  ssmask = readNIfTI(maskfile, 
                     reorient = reorient)
  
  #############################
  # Filling the mask
  #############################   
  ssmask = dil_ero(ssmask, 
                    retimg = TRUE,
                    nvoxels = nvoxels)
  
  ss = maskimg(img, ssmask)
  
  writeNIfTI(ss, 
             filename = outfile)
  
  #############################
  # Removing mask if keepmask = FALSE
  #############################
  if (!keepmask){
    if (verbose){
      cat("# Removing Mask File\n")
    }
    maskfile = nii.stub(maskfile)
    ext = get.imgext()
    maskfile = paste0(maskfile, ext)
    file.remove(maskfile)
  }

  return(ss)
}