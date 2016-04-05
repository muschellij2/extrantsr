#' @title Image Entropy using histograms
#'
#' @description Get the entropy for an entire image using histogram binning
#' @param img object of class \code{nifti} or character
#' @param mask binary object of class \code{nifti} or character
#' @param breaks Number of breaks to pass to \code{\link{hist}}
#' @param base Base to take the log for the entropy.  Default is e
#' @param ... Additional arguments to pass to \code{hist}.  Plot has already been
#' set to \code{FALSE}
#' @examples
#' library(oro.nifti)
#' arr = array(rnorm(100^3), dim = rep(100, 3))
#' img = oro.nifti::nifti(arr)
#' img_hist_entropy(img)
#' @export
#' @import fslr
img_hist_entropy <- function(img, mask = NULL, 
                            breaks = 2000, 
                            base = exp(1), ...) {
  img = check_nifti(img)
  if (!is.null(mask)){
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
  h = hist(vals, breaks = breaks, plot = FALSE, ...)
  counts = h$counts
  counts = counts[ counts != 0 ]
  counts = counts / sum(counts)
  ent = - sum( counts * log(counts, base = base))
  return(ent)
}
