#' Get list of transforms that would likely be made with a specific `outprefix`
#'
#' @param outprefix If you used this in \code{\link{antsRegistration}}, then
#' what would be the filenames of the transforms
#' @param only_exists if \code{TRUE}, only those that exists will be returned.
#'
#' @return A list of character vectors
#' @export
#'
#' @examples
#' outprefix = tempfile()
#' transformlist_from_outprefix(outprefix)
transformlist_from_outprefix = function(outprefix,
                                        only_exists = FALSE) {
  out_trans = c(
    Affine = "0GenericAffine.mat", 
    fwd = "1Warp.nii.gz", 
    inv = "1InverseWarp.nii.gz")
  fwdtransforms = paste0(outprefix, out_trans[c("fwd", "Affine")])
  invtransforms = paste0(outprefix, out_trans[c("Affine", "inv")])
  
  if (only_exists) {
    fwdtransforms = fwdtransforms[file.exists(fwdtransforms)]
    invtransforms = invtransforms[file.exists(invtransforms)]
  }
  
  l = list(fwdtransforms = fwdtransforms,
           invtransforms = invtransforms)
  return(l)
}