#' @title Remove Neck from CT Scan
#'
#' @description Removes the neck from axially acquired CT scan so
#' Skull stripping can be done.
#' @param file File for neck removal - either filename or class nifti
#' @param template.file Template to warp to original image space
#' @param template.mask Mask of template to use as rough brain mask
#' @param ret_mask Return mask of slices to keep
#' @param typeofTransform Transformation for template to image, passed to
#' \code{\link{ants_regwrite}}.
#' @param rep.value Value to replace neck slices with
#' @param ... not used
#' @export
#' @return Object of class nifti or vector of indices
remove_neck <- function(file, 
	template.file = NULL,
	template.mask = NULL,
	ret_mask = FALSE,
	typeofTransform = "Rigid",
  rep.value =0,
  ...){

	file = checkimg(file)
	ofile = paste0(tempfile(), '.nii.gz')
  if (is.null(template.file)){
    cat("Potential atlases are at\n ")
    cat(paste0('system.file("scct_unsmooth.nii.gz", package="cttools")\n'))
    cat(paste0('file.path( fsldir(), "data/standard", ', 
               '"MNI152_T1_1mm_brain.nii.gz")\n'))
    stop("Need template.file specified!")
  }
  template.file = checkimg(template.file)
  if (is.null(template.mask)){
    template.mask = fslbin(file=template.file, retimg=TRUE)
  } 
	template.mask = checkimg(template.mask)
	
	ret = ants_regwrite(filename = template.file, template.file = file, 
		typeofTransform=typeofTransform, other.files = template.mask, 
		other.outfiles = ofile, retimg = TRUE, remove.warp = TRUE)

	img = readNIfTI(file, reorient=FALSE)
	mask = readNIfTI(ofile, reorient=FALSE)

	ind = which(mask > 0.5, arr.ind=TRUE)
	#5mm
	# dimg = dim(img)
	minz = min(ind[,"dim3"])
	if (ret_mask) {
    inds = seq(minz, dim(img)[3])
    newimg = array(0, dim = dim(img))
    newimg[,,inds] = 1	  
    newimg = niftiarr(img, newimg)
	} else {
	  inds = seq(1, minz-1)
	  newimg = img
	  newimg@.Data[,,inds] = rep.value	  
	  newimg = cal_img(newimg)
	}
	return(newimg)
}
