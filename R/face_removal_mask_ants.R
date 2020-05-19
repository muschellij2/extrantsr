#' Face Removal Mask using ANTs Registration
#'
#' @param file input image 
#' @param template Template image  to register input image to.  Set 
#' to \code{NULL} (recommended)  if want to use from 
#' \url{https://github.com/poldracklab/pydeface}.
#' Alternatively, use \code{\link{mni_fname}}.
#' @param face_mask Mask of image, in same space as \code{template}.
#' Set to \code{NULL} (recommended) if want to use from 
#' \url{https://github.com/poldracklab/pydeface}. 
#' Alternatively, use \code{mni_face_fname} from \code{fsl}.
#' @param outfile Output file name
#' @param retimg (logical) return image of class nifti
#' @param ... additional arguments to pass to 
#' \code{\link{registration}}
#'
#' @return An image or filename depending on \code{retimg}
#' @export
#'
#' @examples
#' \donttest{
#' if (fslr::have_fsl()) {
#'    file = "~/Downloads/sample_T1_input.nii.gz"
#'    if (file.exists(file)) {
#'        mask = face_removal_mask_ants(file = file,
#'          template = NULL, face_mask = NULL)
#'        image = mask_img(file, mask)
#'    }
#' }
#' }
face_removal_mask_ants = function(
  file, 
  template = NULL, 
  face_mask = NULL,
  outfile = NULL,
  ...,
  retimg = FALSE)  {
  
  if (is.null(template) & is.null(face_mask)) {
    base_url = "https://github.com/poldracklab/pydeface/raw/master/pydeface/data"
    face_mask = tempfile(fileext = ".nii.gz")
    download.file(file.path(base_url, "facemask.nii.gz"), 
                  destfile = face_mask)
    template = tempfile(fileext = ".nii.gz")
    download.file(file.path(base_url, "mean_reg2mean.nii.gz"), 
                  destfile = template)
  }

  if (is.null(outfile)) {
    outfile = tempfile(fileext = ".nii.gz")
  }
  reg = registration(
    filename = template, 
    template.file = file, 
    interpolator = "Linear", 
    typeofTransform = "Affine", ...)
  
  face_mask = check_ants(face_mask)
  # to avoid anti-aliasing with background
  face_mask = face_mask + 1
  mask = ants_apply_transforms(
    moving = face_mask, fixed = file,
    transformlist = reg$fwdtransforms,
    interpolator = "NearestNeighbor") 
  mask = mask != 1
  writenii(mask, outfile)
  if (retimg) {
    return(mask)
  }
  return(outfile)
}

#' @export
#' @rdname face_removal_mask_ants
deface_image_ants = function(file, ...) {
  
  args = list(...)
  retimg  = args$retimg
  if (is.null(retimg)) {
    retimg = FALSE
  }
  args$file = file
  args$retimg = TRUE
  mask = do.call(face_removal_mask_ants, args = args)
  out = mask_img(file, mask = mask)
  if (!retimg) {
    out = tempimg(out)
  }
  out
}