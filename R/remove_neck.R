#' @title Remove Neck from CT Scan
#'
#' @description Removes the neck from axially acquired CT scan so
#' Skull stripping can be done.
#' @param file File for neck removal - either filename or class nifti
#' @param template.file Template to warp to original image space
#' @param template.mask Mask of template to use as rough brain mask
#' @param ret_mask Return mask of slices to keep
#' @param rep.value Value to replace neck slices with
#' @export
#' @importFrom cttools install_dcm2nii
#' @return Object of class nifti or vector of indices
remove_neck <- function(file, 
	template.file = system.file("scct_unsmooth.nii.gz", 
		package="cttools"),
	template.mask = system.file("scct_unsmooth_Skull_Stripped_Mask.nii.gz", 
		package="cttools",
  ret_mask = FALSE),
  rep.value =0 ){

	file = checkimg(file)
	ofile = paste0(tempfile(), '.nii.gz')
	ret = ants_regwrite(filename = template.file, template.file = file, 
		typeofTransform="Rigid", other.files = template.mask, 
		other.outfiles = ofile, retimg = TRUE, remove.warp = TRUE)

	img = readNIfTI(file, reorient=FALSE)
	mask = readNIfTI(ofile, reorient=FALSE)

	ind = which(mask > 0.5, arr.ind=TRUE)
	#5mm
	# dimg = dim(img)
	minz = min(ind[,"dim3"])
	inds = seq(1, minz-1)
	if (ret_mask) {
    newimg = array(0, dim = dim(img))
    newimg[,,inds] = 1	  
    newimg = niftiarr(img, newimg)
	} else {
	  newimg = img
	  newimg@.Data[,,inds] = rep.value	  
	  newimg = cal_img(newimg)
	}
	return(newimg)
}
