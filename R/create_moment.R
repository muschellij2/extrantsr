#' @title Create Neighborhood Momemts of an Image
#' @description Creates first (mean) through fourth (kurtosis) central moments,
#' as well as gradients and mean/sd image.
#' @param img Object of class \code{nifti}, \code{character} or \code{antsImage}
#' @param mask Object of class \code{nifti}, \code{character} or \code{antsImage}.
#' Only neighborhoods inside the mask will be taken.  If \code{NULL}, all
#' neighbors of the image will be taken
#' @param radius array of values for neighborhood radius (in voxels)
#' @param retimg Should images (of class \code{nifti}) be returned or should
#' a matrix
#' @param verbose print diagnostic messages
#' @param ... arguments to passed to \code{\link{neighborhood}}
#'
#' @return List of images (of class \code{nifti}) or
#' a matrix (V by 6), where V is the number of voxels in the mask
#' @export
#' @importFrom plyr alply laply
#' @import oro.nifti
#' @importFrom matrixStats colCounts
create_moment = function(img,
                         mask = NULL,
                         radius = rep(1,3),
                         retimg = TRUE,
                         verbose = TRUE,
                         ...){

  img = check_ants(img)
  img.dim = dim(img)
  if (is.null(mask)) {
    mask = array(1, dim = img.dim)
    mask = as.antsImage(mask)
    mask = antsCopyImageInfo(target = mask, reference = img)
  }

  if (verbose) {
    message("Getting Neighborhood")
  }
  grads = neighborhood(
    img = img,
    mask = mask,
    radius = radius,
    spatial.info = TRUE,
    get.gradient = TRUE,
    verbose = verbose,
    ...)
  mat = grads$values
  if (verbose) {
    message("Counting Missing Values")
  }
  nNA <- colCounts(mat,
                   value = NA_real_,
                   na.rm = FALSE)
  n <- nrow(mat) - nNA
  mn = colMeans(mat, na.rm = TRUE)
  tmat = t(mat)

  if (verbose) {
    message("Creating Moments")
  }
  s2 = 1/n * rowSums((tmat - mn) ^ 2, na.rm = TRUE)
  v = s2 * n/(n - 1)
  s3 = 1/n * rowSums((tmat - mn) ^ 3, na.rm = TRUE)
  s4 = 1/n * rowSums((tmat - mn) ^ 4, na.rm = TRUE)

  if (verbose) {
    message("Creating Skew/Kurtosis")
  }
  kurt = s4 / (s2 ^ 2) - 3
  skew = s3 / (v) ^ {3/2}
  sd = sqrt(v)

  ################################
  # Get Gradient and reorder indices
  ################################
  if (verbose) {
    message("Creating Large Matrix")
  }
  grad = colSums(grads$gradients ^ 2)
  L = cbind(mn, sd, skew, kurt, grad)
  L = cbind(L, z = mn / sd)
  L[!is.finite(L)] = 0

  mask = check_nifti(mask)
  fmask = datatyper(mask,
                    datatype =
                      convert.datatype()$FLOAT32,
                    bitpix =
                      convert.bitpix()$FLOAT32)
  if (retimg) {
    if (verbose) {
      message("Remaking Images")
    }
    L = alply(.data = L,
              .margins = 2, .fun = remake_img,
              img = fmask,
              mask = mask,
              .progress = ifelse(verbose, "text", "none"))
    names(L) = as.character(
      attr(L,"split_labels")[,1]
    )
  }
  return(L)
}