#' @title Remove Neck from Image
#'
#' @description Removes the neck from axially acquired scan so
#' skull stripping can be done.
#' @param file File for neck removal - either filename or class nifti
#' @param template.file Template to warp to original image space
#' @param template.mask Mask of template to use as rough brain mask.  If
#' \code{template.file} is specified, but \code{template.mask} is not,
#' then \code{fslbin(file=template.file)} is performed.
#' @param ret_mask Return mask of slices to keep
#' @param typeofTransform Transformation for template to image, passed to
#' \code{\link{ants_regwrite}}.
#' @param rep.value Value to replace neck slices with
#' @param swapdim Should the dimensions be swapped before registration,
#' and then reset after
#' @param verbose Print out diagnostic messages
#' @param ... not used
#' @export
#' @return Object of class nifti or vector of indices
#' @importFrom fslr fslbin rpi_orient reverse_rpi_orient
remove_neck <- function(file,
                        template.file,
                        template.mask = NULL,
                        ret_mask = FALSE,
                        typeofTransform = "Rigid",
                        rep.value =0,
                        swapdim = TRUE,
                        verbose = TRUE,
                        ...){

  file = checkimg(file)
  ofile = tempfile(fileext = '.nii.gz')

  if (missing(template.file)) {
    message("Potential atlases are at\n ")
    message(paste0('system.file("scct_unsmooth.nii.gz", package="ichseg")\n'))
    message(paste0('file.path( fsldir(), "data/standard", ',
                   '"MNI152_T1_1mm_brain.nii.gz")\n'))
    stop("Need template.file specified!")
  }
  template.file = checkimg(template.file)
  if (is.null(template.mask)) {
    if (verbose) {
      message("# Creating Binary Template mask using fslbin\n")
    }
    template.mask = fslbin(file = template.file, retimg = TRUE, 
                           verbose = verbose)
  }
  template.mask = checkimg(template.mask)

  if (verbose) {
    message("# Registration to template\n")
  }
  if (swapdim) {
    if (verbose) {
      message(paste0("# Swapping Dimensions \n"))
    }
    # forms = getForms(file)
    # if (forms$sform_code == 0 & forms$qform_code == 0) {
    #   stop("Cannot swap dimensions - sform_code and qform_code are 0!")
    # }
    # if (forms$sform_code != 0) {
    #   sorient = forms$ssor
    # } else {
    #   sorient = forms$sqor
    # }
    # ori = fslgetorient(file)
    # if (ori == "NEUROLOGICAL") {
    #   # need to copy because fslorient samefile stuff
    #   tdir = tempfile()
    #   dir.create(tdir, showWarnings = verbose)
    #   tfile = file.path(tdir,
    #                     basename(file))
    #   file.copy(file, tfile, overwrite = TRUE)
    #   # changes from NEUROLOGICAL to RADIOLOGICAL
    #   file = fslorient(tfile,
    #                    opts = "-swaporient",
    #                    retimg = TRUE,
    #                    verbose = verbose)
    # }
    # # Changes the data
    # file = fslswapdim(file = file,
    #                   retimg = TRUE,
    #                   a = "RL", b = "PA", c = "IS",
    #                   verbose = verbose)
    L = rpi_orient(file, verbose = verbose)
    file = L$img
    sorient = L$orientation
    ori = L$convention
  }
  ants_regwrite(filename = template.file,
                template.file = file,
                typeofTransform = typeofTransform,
                other.files = template.mask,
                other.outfiles = ofile,
                retimg = TRUE,
                remove.warp = TRUE,
                # added 2017May04
                interpolator = "nearestNeighbor",
                verbose = verbose)

  if (verbose) {
    message("# Reading in Transformed data\n")
  }
  img = check_nifti(file)
  mask = readnii(ofile, reorient = FALSE)

  ind = which(mask > 0.5, arr.ind = TRUE)
  #5mm
  # dimg = dim(img)
  if (verbose) {
    message("# Dropping slices not in mask\n")
  }
  minz = min(ind[,"dim3"])
  if (ret_mask) {
    inds = seq(minz, dim(img)[3])
    newimg = array(0, dim = dim(img))
    newimg[,,inds] = 1
    newimg = niftiarr(img, newimg)
  } else {
    inds = seq(1, minz - 1)
    newimg = img
    newimg@.Data[,,inds] = rep.value
    newimg = cal_img(newimg)
  }
  if (swapdim) {
    if (verbose) {
      message(paste0("# Swapping Dimensions Back\n"))
    }
    newimg = reverse_rpi_orient(file = newimg,
                                convention = ori,
                                orientation = sorient,
                                verbose = verbose)
  }
  return(newimg)
}


#' @title Remove Neck Twice from a Scan
#'
#' @description Wrapper for running \code{remove_neck} twice, as some images
#' require this if large number of slices of the neck or upper shoulders
#' were scanned
#' @param file File for neck removal - either filename or class nifti
#' @param template.file Template to warp to original image space
#' @param template.mask Mask of template to use as rough brain mask.  If
#' \code{template.file} is specified, but \code{template.mask} is not,
#' then \code{fslbin(file=template.file)} is performed.
#' @param typeofTransform Transformation for template to image, passed to
#' \code{\link{ants_regwrite}}.
#' @param rep.value Value to replace neck slices with
#' @param swapdim Should the dimensions be swapped before registration,
#' and then reset after
#' @param verbose Print out diagnostic messages
#' @param ret_mask Should the mask be returned vs. the image?
#' @export
#' @return Object of class nifti
double_remove_neck = function(
  file,
  template.file,
  template.mask,
  typeofTransform = "Rigid",
  rep.value = 0,
  swapdim = TRUE,
  verbose = TRUE,
  ret_mask = FALSE) {

  ########################
  # removing neck once
  ########################
  noneck = remove_neck(
    file = file,
    template.file = template.file,
    template.mask = template.mask,
    typeofTransform = typeofTransform,
    rep.value = rep.value,
    swapdim = swapdim,
    verbose = verbose,
    ret_mask = ret_mask)

  if (ret_mask) {
    file = check_nifti(file)
    noneck = mask_img(img = file, mask = noneck)
  }
  ########################
  # removing neck twice
  ########################
  noneck2 = remove_neck(
    file = noneck,
    template.file = template.file,
    template.mask = template.mask,
    typeofTransform = typeofTransform,
    rep.value = rep.value,
    swapdim = swapdim,
    verbose = verbose,
    ret_mask = ret_mask)
  return(noneck2)
}
