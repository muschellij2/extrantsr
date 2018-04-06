#' Compute Image Similarity Metrics
#'
#' @param fixed Fixed image (not moving)
#' @param moving Moving image (moved into fixed space)
#' @param fixed_mask Mask for fixed image (optional)
#' @param moving_mask Mask for moving image (optional)
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
#' image_similarity(x,y, type="Correlation")
#' image_similarity(x,y, type="MattesMutualInformation")
#' image_similarity(x,y, type="ANTSNeighborhoodCorrelation")
#' image_similarity(x,y, type="JointHistogramMutualInformation")
#' image_similarity(x,y, type="Demons")
#'
image_similarity = function(
  fixed, moving,
  fixed_mask = NULL,
  moving_mask = NULL,
  ...
) {
  fixed = check_ants(fixed)
  moving = check_ants(moving)
  if (!is.null(fixed_mask)) {
    fixed.mask = check_ants(fixed_mask)
  } else {
    fixed.mask = NA
  }
  if (!is.null(moving_mask)) {
    moving.mask = check_ants(moving_mask)
  } else {
    moving.mask = NA
  }

  res = ANTsRCore::imageSimilarity(
    fixed = fixed, moving = moving,
    fixed.mask=fixed.mask, moving.mask=moving.mask,
    ...)
  return(res)
}
