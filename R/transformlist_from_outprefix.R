#' Get list of transforms that would likely be made with a specific `outprefix`
#'
#' @param outprefix If you used this in \code{\link{antsRegistration}}, then
#' what would be the filenames of the transforms
#' @param typeofTransform transformation to be used in registration
#' @param only_exists if \code{TRUE}, only those that exists will be returned.
#'
#' @return A list of character vectors
#' @export
#'
#' @examples
#' outprefix = tempfile()
#' transformlist_from_outprefix(outprefix)
#' transformlist_from_outprefix(outprefix, typeofTransform = "Rigid")
#' res = transformlist_from_outprefix(outprefix, typeofTransform = "Rigid",
#' only_exists = TRUE)
#' stopifnot(is.null(res))
transformlist_from_outprefix = function(
  outprefix,
  typeofTransform = "SyN",
  only_exists = FALSE) {
  if (is.null(typeofTransform)) {
    typeofTransform = "SyN"
  }
  have_warp = (typeofTransform %in%
                 c("Translation", "Rigid", "Similarity", "TRSAA")) |
    grepl("Rigid", typeofTransform) |
    grepl("Affine", typeofTransform)
  have_warp = !have_warp
  
  out_trans = list(
    Affine = "0GenericAffine.mat", 
    fwd = "1Warp.nii.gz",
    inv = "1InverseWarp.nii.gz"
  )
  if (!have_warp) {
    out_trans$fwd = out_trans$inv = NULL
  }
  out_trans = out_trans[ out_trans != ""]
  fwdtransforms = paste0(outprefix, c(out_trans$fwd, out_trans$Affine))
  invtransforms = paste0(outprefix, c(out_trans$Affine, out_trans$inv))
  
  if (only_exists) {
    fwdtransforms = fwdtransforms[file.exists(fwdtransforms)]
    if (length(fwdtransforms) == 0) {
      fwdtransforms = NULL
    }
    invtransforms = invtransforms[file.exists(invtransforms)]
    if (length(invtransforms) == 0) {
      invtransforms = NULL
    }    
  }
  
  l = list()
  l$fwdtransforms = fwdtransforms
  l$invtransforms = invtransforms
  if (length(l) == 0) {
    return(NULL)
  }
  return(l)
}