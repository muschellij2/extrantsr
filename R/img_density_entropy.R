#' @title Image Entropy using density
#'
#' @description Get the entropy for an entire image using density binning
#' @param img object of class \code{nifti} or character
#' @param mask binary object of class \code{nifti} or character
#' @param base Base to take the log for the entropy.  Default is e
#' @param ... Additional arguments to pass to \code{\link{density}}.  
#' @examples
#' library(oro.nifti)
#' arr = array(rnorm(100^3), dim = rep(100, 3))
#' img = oro.nifti::nifti(arr)
#' img_density_entropy(img)
#' @export
#' @importFrom stats density
img_density_entropy <- function(img, 
                                mask = NULL, 
                                base = exp(1), ...) {
  img = check_nifti(img)
  if (!is.null(mask)) {
    mask = check_nifti(mask)
    allowable = c(0, 1)
    mask = as(mask, "array")
    class(mask) = "numeric"
    umask = unique(c(mask))
    if (!all(umask %in% allowable)) {
      stop("Mask must be binary 0/1.")
    }
    vals = img[mask == 1]
  } else {
    vals = c(img)
  }
  h = density(vals, ...)
  counts = h$y
  counts = counts[ counts != 0 ]
  counts = counts / sum(counts)
  ent = -sum( counts * log(counts, base = base))
  return(ent)
}
