#' @name apply_multi_reg-methods
#' @docType methods 
#' @aliases apply_multi_reg 
#' @title Check if antsImage or read in
#' @description Simple check to see if input is character, list, \code{nifti},
#' or class \code{antsImage}
#' @param infiles Input image files
#' @param transformlist list or character vector of transforms generated 
#' by antsRegistration where each transform is a filename. 
#' @param fixed fixed image defining domain into which the infiles image 
#' is transformed
#' @param dimension (numeric) passed to 
#' \code{\link{antsImageRead}} if the image
#' is read in
#' @param ... Arguments passed to \code{\link{antsApplyTransforms}}
#' @export 
#' @return Object of class \code{\link{nifti}}
#' @seealso \code{\link{antsApplyTransforms}} 
#' @import methods
#' @author John Muschelli \email{muschellij2@@gmail.com}  
setGeneric("apply_multi_reg", function(
  infiles,
  transformlist,
  fixed,
  ...)  {
  standardGeneric("apply_multi_reg")
})

#' @rdname apply_multi_reg-methods
#' @aliases apply_multi_reg,list,list-method
#' @export
setMethod("apply_multi_reg", 
          signature("list", "list"), 
          function(
            infiles,
            transformlist,
            fixed,
            ...) { 
            stopifnot(length(infiles) == length(transformlist))
            
            infiles = check_ants(infiles)
            fixed = check_ants(fixed)
            res = mapply(function(x, y){
              ants_apply_transforms(fixed = fixed, 
                                    moving = x,
                                    transformlist = y,
                                    ...)
            }, SIMPLIFY = FALSE)
            
            return(res)
          })

#' @rdname apply_multi_reg-methods
#' @aliases apply_multi_reg,character,list-method
#' @export
setMethod("apply_multi_reg", 
          signature("character", "list"), 
          function(
            infiles,
            transformlist,
            fixed,
            ...) { 
            stopifnot(length(infiles) == length(transformlist))
            
            infiles = check_ants(infiles)
            fixed = check_ants(fixed)
            res = mapply(function(x, y){
              ants_apply_transforms(fixed = fixed, 
                                    moving = x,
                                    transformlist = y,
                                    ...)
            }, SIMPLIFY = FALSE)
            
            return(res)
          })



#' @rdname apply_multi_reg-methods
#' @aliases apply_multi_reg,antsImage,character-method
#' @export
setMethod("apply_multi_reg", 
          signature("antsImage", "character"), 
          function(
            infiles,
            transformlist,
            fixed,
            ...) { 
            
            fixed = check_ants(fixed)
            res = ants_apply_transforms(fixed = fixed, 
                                        moving = infiles,
                                        transformlist = transformlist,
                                        ...)
            
            return(res)
          })

#' @rdname apply_multi_reg-methods
#' @aliases apply_multi_reg,list,character-method
#' @export
setMethod("apply_multi_reg", 
          signature("nifti", "character"), 
          function(
            infiles,
            transformlist,
            fixed,
            ...) { 
            
            fixed = check_ants(fixed)
            res = ants_apply_transforms(fixed = fixed, 
                                        moving = infiles,
                                        transformlist = transformlist,
                                        ...)
            
            return(res)
          })