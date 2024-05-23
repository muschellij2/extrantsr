#' @name resample_to_target-methods
#' @docType methods 
#' @aliases resample_to_target 
#' @title Resample an Image to a Target Image
#' @description Resamples an \code{antsImage} or 
#' \code{\link{nifti}}
#' object to a target
#' 
#' @return An \code{antsImage} or \code{\link{nifti}} depending on
#' input
#' 
#' @param img character path of image or 
#' an object of class nifti, or antsImage
#' @param target image of reference,
#'  the output will be in this space.  
#'  Will be coerced using \code{\link{check_ants}}
#' @param interpolator what type of interpolation should be
#' done
#' @param copy_origin Copy image origin from \code{target}, 
#' using \code{\link{antsCopyOrigin}}
#' @param ... additional arguments to pass to 
#' \code{\link{resampleImageToTarget}}
#' 
#' @export 
#' @examples 
#' library(oro.nifti)
#' library(extrantsr)
#' n = 30
#' x = nifti(array(rnorm(n^3*10), dim = c(n, n, n)))
#' new_pixdim = c(0.5, 0.5, 0.5)
#' res = resample_image(x, parameters = new_pixdim)
#' x2 = resample_to_target(x, res)
#' pixdim(x2)[2:4]
#' stopifnot(all(pixdim(x2)[2:4] == new_pixdim))
setGeneric("resample_to_target", function(
  img, 
  target, 
  interpolator = c(
    "linear", 
    "nearestNeighbor", 
    "multiLabel",
    "gaussian",
    "bSpline",
    "cosineWindowedSinc",
    "welchWindowedSinc",
    "hammingWindowedSinc",
    "lanczosWindowedSinc", 
    "genericLabel"), 
  copy_origin = FALSE,
  ...
){
  standardGeneric("resample_to_target")
})


#' @rdname resample_to_target-methods
#' @aliases resample_to_target,character-method
#'  
#' @export
setMethod(
  "resample_to_target", "character", 
  function(
    img, 
    target, 
    interpolator = c(
      "linear", 
      "nearestNeighbor", 
      "multiLabel",
      "gaussian",
      "bSpline",
      "cosineWindowedSinc",
      "welchWindowedSinc",
      "hammingWindowedSinc",
      "lanczosWindowedSinc", 
      "genericLabel"), 
    copy_origin = FALSE,
    ...) { 
    
    img = antsImageRead(img)
    res = .resample_to_target(
      img = img, 
      target = target,
      interpolator = interpolator,
      copy_origin = copy_origin,
      ...)
    newimg = ants2oro(res)
    return(newimg)
  })

#' @rdname resample_to_target-methods
#' @aliases resample_to_target,nifti-method
#'  
#' @export
setMethod(
  "resample_to_target", "nifti", 
  function(
    img, 
    target, 
    interpolator = c(
      "linear", 
      "nearestNeighbor", 
      "multiLabel",
      "gaussian",
      "bSpline",
      "cosineWindowedSinc",
      "welchWindowedSinc",
      "hammingWindowedSinc",
      "lanczosWindowedSinc", 
      "genericLabel"), 
    copy_origin = FALSE,
    ...) { 
    tmp_img = oro2ants(img)
    rm(list = "img"); gc()
    res = .resample_to_target(
      img = tmp_img, 
      target = target,
      interpolator = interpolator,
      copy_origin = copy_origin,
      ...)
    newimg = ants2oro(res)    
    rm(list = c("tmp_img", "res")); gc(); gc();
    return(newimg)
  })

#' @rdname resample_to_target-methods
#' @aliases resample_to_target,antsImage-method
#'  
#' @export
setMethod(
  "resample_to_target", "antsImage", 
  function(
    img, 
    target,   
    interpolator = c(
      "linear", 
      "nearestNeighbor", 
      "multiLabel",
      "gaussian",
      "bSpline",
      "cosineWindowedSinc",
      "welchWindowedSinc",
      "hammingWindowedSinc",
      "lanczosWindowedSinc", 
      "genericLabel"), 
    copy_origin = FALSE,
    ...) { 
    
    newimg = .resample_to_target(
      img = img, 
      target = target,
      interpolator = interpolator,    
      copy_origin = copy_origin,
      ...)
    return(newimg)
  })



#' @rdname resample_to_target-methods
.resample_to_target = function(
  img,   
  target, 
  interpolator = c(
    "linear", 
    "nearestNeighbor", 
    "multiLabel",
    "gaussian",
    "bSpline",
    "cosineWindowedSinc",
    "welchWindowedSinc",
    "hammingWindowedSinc",
    "lanczosWindowedSinc", 
    "genericLabel"), 
  copy_origin = FALSE,
  ...) {
  
  target = check_ants(target)
  
  if (copy_origin) {
    img = antsCopyOrigin(target = img, reference = target)
  }
  
  interpolator = match.arg(interpolator)
  interpType = interpolator
  res_img = resampleImageToTarget(
    image = img, 
    target = target,
    interpType = interpType,
    ...)
  return(res_img)
}