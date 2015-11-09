#' @name ants_apply_transforms-methods
#' @docType methods 
#' @aliases ants_apply_transforms 
#' @title Apply ANTs transforms to nifti images
#'
#' @description Wraps \code{\link{antsApplyTransforms}} but allows for 
#' \code{\link{nifti}} objects from the \code{oro.nifti} package
#' @param fixed image defining domain into which the moving image is transformed.
#' @param moving image to be mapped to fixed space.
#' @param ... Arguments to be passed to \code{\link{antsApplyTransforms}}. 
#' @export 
#' @return Either \code{nifti} object or list of nifti objects
#' @seealso \code{\link{antsApplyTransforms}} 
#' @import methods
#' @author John Muschelli \email{muschellij2@@gmail.com}  
setGeneric("ants_apply_transforms", function(fixed, moving, ...)  {
  standardGeneric("ants_apply_transforms")
})

#' @rdname ants_apply_transforms-methods
#' @aliases ants_apply_transforms,ANY,list-method
#' @export
setMethod("ants_apply_transforms", 
          signature("ANY", "list"), 
          function(fixed, moving, ...) { 
            fixed = check_ants(fixed)
            moving = check_ants(moving)
            res = lapply(moving, ants_apply_transforms, 
                         fixed = fixed, ... = ...)
            return(res)
          })

#' @rdname ants_apply_transforms-methods
#' @aliases ants_apply_transforms,ANY,character-method
#' @export
setMethod("ants_apply_transforms", 
          signature("ANY", "character"), 
          function(fixed, moving, ...) { 
            fixed = check_ants(fixed)
            moving = check_ants(moving)
            if (length(moving) > 1) {
            res = lapply(moving, ants_apply_transforms, 
                         fixed = fixed, ... = ...)
            } else {
              res = ants_apply_transforms(fixed = fixed, 
                                          moving = moving, 
                                          ... = ...)
            }
            return(res)
          })


#' @rdname ants_apply_transforms-methods
#' @aliases ants_apply_transforms,ANY,ANY-method
#' @export
setMethod("ants_apply_transforms", 
          signature("ANY", "ANY"), 
          function(fixed, moving, ...) { 
            
            fixed = check_ants(fixed)
            moving = check_ants(moving)
            res = ants_apply_transforms(fixed = fixed, 
                                        moving = moving,
                                        ...)
            
            return(res)
          })
