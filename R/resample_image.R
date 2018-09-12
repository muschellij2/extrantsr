#' @name resample_image-methods
#' @docType methods 
#' @aliases resample_image 
#' @title Resample an image
#' @description Resamples an \code{antsImage} or 
#' \code{\link{nifti}}
#' object
#' 
#' @return An \code{antsImage} or \code{\link{nifti}} depending on
#' input
#' 
#' @param img character path of image or 
#' an object of class nifti, or antsImage
#' @param parameters parameters
#'  to pass as \code{resampleParams} in
#'  \code{\link{resampleImage}}
#' @param parameter_type Are the parameters in units of 
#' voxels or millimeters (mm)
#' @param interpolator what type of interpolation should be
#' done
#' @param ... not currently used
#' 
#' @export 
#' @examples 
#' library(oro.nifti)
#' library(extrantsr)
#' n = 30
#' x = nifti(array(rnorm(n^3*10), dim = c(n, n, n)))
#' res = resample_image(x, parameters = c(0.5, 0.5, 0.5))
#' res
#' @importFrom ANTsRCore resampleImage
setGeneric("resample_image", function(
  img, parameters, 
  parameter_type = c("mm", "voxels"), 
  interpolator = c("nearestneighbor", "linear", 
                   "gaussian", "windowedsinc", 
                   "bspline"), ...
){
  standardGeneric("resample_image")
})


#' @rdname resample_image-methods
#' @aliases resample_image,character-method
#'  
#' @export
setMethod(
  "resample_image", "character", 
  function(
    img, 
    parameters, 
    parameter_type = c("mm", "voxels"), 
    interpolator = c("nearestneighbor", "linear", 
                     "gaussian", "windowedsinc", 
                     "bspline"),
    ...) { 
    
    img = antsImageRead(img)
    res = .resample_image(
      img = img, 
      parameters = parameters,
      parameter_type = parameter_type,
      interpolator = interpolator,
      ...)
    newimg = ants2oro(res)
    return(newimg)
  })

#' @rdname resample_image-methods
#' @aliases resample_image,nifti-method
#'  
#' @export
setMethod(
  "resample_image", "nifti", 
  function(
    img, parameters, 
    parameter_type = c("mm", "voxels"), 
    interpolator = c("nearestneighbor", "linear", 
                     "gaussian", "windowedsinc", 
                     "bspline"), ...) { 
    tmp_img = oro2ants(img)
    rm(list = "img"); gc
    res = .resample_image(
      img = tmp_img, 
      parameters = parameters,
      parameter_type = parameter_type,
      interpolator = interpolator,
      ...)
    newimg = ants2oro(res)
    rm(list = c("tmp_img", "res")); gc(); gc();
    return(newimg)
  })

#' @rdname resample_image-methods
#' @aliases resample_image,antsImage-method
#'  
#' @export
setMethod(
  "resample_image", "antsImage", 
  function(img, parameters, 
           parameter_type = c("mm", "voxels"), 
           interpolator = c("nearestneighbor", "linear", 
                            "gaussian", "windowedsinc", 
                            "bspline"), ...) { 
    
    newimg = .resample_image(
      img = img, 
      parameters = parameters,
      parameter_type = parameter_type,
      interpolator = interpolator,      
      ...)
    return(newimg)
  })



#' @rdname resample_image-methods
.resample_image = function(
  img, parameters, 
  parameter_type = c("mm", "voxels"), 
  interpolator = c("nearestneighbor", "linear", 
                   "gaussian", "windowedsinc", 
                   "bspline")) {
  interpolator = tolower(interpolator)
  parameter_type = match.arg(parameter_type)
  useVoxels = parameter_type == "voxels"
  
  interpolator = match.arg(interpolator)
  interpolator = factor(
    interpolator, 
    levels = c("linear", "nearestneighbor",  
               "gaussian", "windowedsinc", 
               "bspline"))
  interpType = as.integer(interpolator) - 1
  res_img = ANTsRCore::resampleImage(
    image = img, 
    resampleParams = parameters,
    useVoxels = useVoxels,
    interpType = interpType)
  return(res_img)
}