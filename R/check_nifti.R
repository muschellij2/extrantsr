#' @rdname check_nifti-methods
#' @aliases check_nifti,antsImage-method
#' @docType methods 
#' @title Check if nifti image or read in a nifti image
#' @description Simple check to see if input is character or of 
#' class nifti
#' @return nifti object or array if allow.array=TRUE and x is an array
#' @seealso \link{readnii}
#' @param x character path of image or 
#' an object of class nifti, or array
#' @param reorient (logical) passed to \code{\link{readnii}} 
#' if the image
#' is to be re-oriented
#' @param allow.array (logical) Are array types allowed (TRUE) or
#' should there be an error if the object is not character or class
#' nifti.
#' @param fast not used for \code{antsImage} classes
#' @param need_header if \code{TRUE}, then an image type with header information
#' will be returned.  If not, then an array is fine.  Used really only in 
#' conjunction with \code{allow.array} 
#' @param ... additional options to pass to \code{\link{ants2oro}}
#' @export
#' @importFrom neurobase check_nifti
setMethod("check_nifti", "antsImage", 
          function(x, 
                   reorient=FALSE, 
                   allow.array=FALSE,
                   fast = FALSE,
                   need_header = TRUE,
                   ...) { 
            if (allow.array & !need_header) {
              x = as.array(x)
            } else {
              x = ants2oro(x, reorient = reorient, ...)
            }
            x
          })
