#' @title Calculates Mutual Information on nifti objects
#'
#' @description Images are converted \code{nifti} objects
#' (from the oro.nifti package), an operation is performed using 
#' \code{\link{antsImageMutualInformation}}
#' @param image1 Object of class \code{nifti} or character filename
#' @param image2 Object of class \code{nifti} or character filename
#' @param ... Additional arguments passed to 
#' \code{\link{antsImageMutualInformation}}
#' @export
#' @importFrom ANTsRCore antsImageMutualInformation
#' @examples
#' library(ANTsR)
#' library(extrantsr)
#' fi = getANTsRData("r16")
#' mi = getANTsRData("r64")
#' mival = mutual_information(fi, mi)
#' @return Numeric
mutual_information <- function(image1, image2, ...){
  image1 = check_ants(image1)
  image2 = check_ants(image2)
  ANTsRCore::antsImageMutualInformation(image1, image2, ...)
}