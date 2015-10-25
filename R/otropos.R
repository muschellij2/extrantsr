#' @title Run Atropos for nifti objects
#'
#' @description Runs Atropos (a finite mixture model) but with more 
#' robust inputs
#' @param a Images to segment.  Can be a \code{nifti} object, 
#' \code{antsImage} object, \code{character} vector, or list of these
#' @param x Mask image.  If none is supplied and \code{make_mask = TRUE},
#' then mask is created by first 
#' @param m mrf parameters as a string, usually 
#' "\code{[smoothingFactor,radius]}".  Reset for 3D images, compared to 
#' 2D default atropos has. 
#' @param ... Additional arguments to be passed to \code{\link{atropos}}
#' @param make_mask Logical indicating if mask should be created if
#' \code{x} is missing.  Mask is made using \code{\link{getMask}} with
#' first image of \code{a}.
#' @param reset_origin Should \code{\link{antsCopyOrigin}} be done
#' on the mask?
#' @export
#' @return Result of \code{\link{atropos}}, but with nifti objects.
otropos <- function(a, 
                    x, 
                    m = "[0.2,1x1x1]", 
                    ..., 
                    make_mask = TRUE,
                    reset_origin = TRUE) {
  if ( is.list(a) | (is.character(a) & length(a) > 0)  ) {
    a = lapply(a, check_ants)
    img = antsImageClone(a[[1]])
  } else {
    a = check_ants(a)
    img = antsImageClone(a)
  }
  
  if  (missing(x)) {
    if (make_mask) {
      x = getMask(img)
      args = list(x = x, ...)
    } else {
      args = list(...)
    }
  } else {
    x = check_ants(x)
    x = antsImageClone(
      (x * 0) + (x > 0), 
      out_pixeltype = "unsigned int")
    if (reset_origin) {
      x = antsCopyOrigin(img, x)
    }
    args = list(x = x, ...)
  }
  args = c(a = a, args, m = m)
  res = do.call(atropos, args)
  if (!all(c("segmentation", "probabilityimages") %in% names(res))) {
      warning(paste0("Results have non-standard output, cowardly ",
                     "returning direct result of res"))
  } else {
      ants_out_seg = ants2oro(res$segmentation)
      ants_out_seg = datatyper(ants_out_seg)
      res$segmentation = ants_out_seg
      for (i in seq_along( res$probabilityimages)) {
        tmp = res$probabilityimages[[i]] 
        tmp = ants2oro(tmp)
        res$probabilityimages[[i]] = tmp
      } 
  }
  return(res)
}