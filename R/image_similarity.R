#' Compute Image Similarity Metrics
#'
#' @param image1 Object of class \code{nifti} or character filename
#' @param image2 Object of class \code{nifti} or character filename
#' @param image1_mask Mask for first image (optional)
#' @param image2_mask Mask for second image (optional)
#' @param ... Additional arguments to pass to
#' \code{\link{imageSimilarity}}
#'
#' @return A numeric value
#' @export
#' @importFrom ANTsRCore imageSimilarity
#'
#' @examples
#' library(ANTsRCore)
#' library(extrantsr)
#' x =  getANTsRData( 'r16' )
#' y =  getANTsRData( 'r30' )
#' image_similarity(x,y, type="MeanSquares")
#' image_similarity(y,x, type="MeanSquares")
#' 
#' image_similarity(x,y, type="Correlation")
#' image_similarity(y,x, type="Correlation")
#' 
#' image_similarity(x,y, type="MattesMutualInformation")
#' image_similarity(y,x, type="MattesMutualInformation")
#' 
#' mutual_information(x, y)
#' mutual_information(y, x)
#' 
#' image_similarity(x,y, type="ANTSNeighborhoodCorrelation")
#' image_similarity(y,x, type="ANTSNeighborhoodCorrelation")
#' image_similarity(x,y, type="JointHistogramMutualInformation")
#' image_similarity(y,x, type="JointHistogramMutualInformation")
#' image_similarity(x,y, type="Demons")
#' image_similarity(y,x, type="Demons")
image_similarity = function(
  image1, image2,
  image1_mask = NULL,
  image2_mask = NULL,
  ...
) {
  fixed = check_ants(image1)
  moving = check_ants(image2)
  if (!is.null(image1_mask)) {
    fixed.mask = check_ants(image1_mask)
  } else {
    fixed.mask = NA
  }
  if (!is.null(image2_mask)) {
    moving.mask = check_ants(image2_mask)
  } else {
    moving.mask = NA
  }

  res = ANTsRCore::imageSimilarity(
    fixed = fixed, moving = moving,
    fixed.mask = fixed.mask, moving.mask = moving.mask,
    ...)
  return(res)
}
