#' @title Apply ANTs transforms to nifti images
#'
#' @description Wraps \code{\link{antsApplyTransforms}} but allows for 
#' \code{\link{nifti}} objects from the \code{oro.nifti} package
#' @param fixed image defining domain into which the moving image is transformed.
#' @param moving image to be mapped to fixed space.
#' @param ... Arguments to be passed to \code{\link{antsApplyTransforms}}. 
#' @export
#' @return Output \code{\link{nifti}} image
ants_apply_transforms <- function(fixed, moving, ...)
{
  fixed = check_ants(fixed)
  moving = check_ants(moving)
  
  moving_to_fixed <- antsApplyTransforms(fixed = fixed, 
                                        moving = moving,
                                        ...) 
  moving_to_fixed = ants2oro(moving_to_fixed)
  
  return(moving_to_fixed)
}

