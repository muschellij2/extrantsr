#' @title Run Atropos for nifti objects
#'
#' @description Runs Atropos (a finite mixture model) but with more 
#' robust inputs
#' @param a Images to segment.  Can be a \code{nifti} object, 
#' \code{antsImage} object, \code{character} vector, or list of these
#' @param x Mask image.  If none is supplied and \code{make_mask = TRUE},
#' then mask is created by first 
#' @param ... Additional arguments to be passed to \code{\link{atropos}}
#' @param make_mask Logical indicating if mask should be created if
#' \code{x} is missing
#' @param reset_origin Should \code{\link{antsCopyOrigin}} be done
#' on the mask?
#' @export
#' @return Result of \code{\link{atropos}}, but with nifti objects.
otropos <- function(a, 
                     x, ..., 
                     make_mask = TRUE,
                     reset_origin = TRUE) {
  if ( is.list(a) | is.character(a)  ) {
    a = lapply(a, check_ants)
    img = antsImageClone(a[[1]])
  } else {
    a = check_ants(a)
    img = antsImageClone(a)
  }
  
  if  (missing(x) ) {
    if (make_mask) {
      x = getMask(img)
      args = list(x = x, ...)
    } else {
      args = list(...)
    }
  } else {
    x = check_ants(x)
    if (reset_origin){
      x = antsCopyOrigin(a, x)
    }
    args = list(x = x, ...)
  }
  args = c(a = a, args)
  res = do.call(atropos, args)
  
  return(res)
}