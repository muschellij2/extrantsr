#' @title Compute cortical thickness using Kelly Kapowski (ANTsR)
#' @description Wrapper for the \code{\link{kellyKapowski}} function
#' in ANTsR, that computes cortical thickness.
#' 
#' @param seg segmentation image
#' @param gray gray matter probability image
#' @param white white matter probability image
#' @param ... arguments passed to \code{\link{kellyKapowski}}
#'
#' @return Object of class \code{nifti}.
#' @export
cort_thickness = function(seg, gray, white,
              ...) {
  
  seg = check_ants(seg)
  gray = check_ants(gray)
  white = check_ants(white)
  
  out = kellyKapowski(s = seg,
                g = gray,
                w = white,
                ...)
  out = ants2oro(out)
  return(out)
}

#' @rdname cort_thickness
#' @export
kk = function(seg, gray, white,
                          ...) {
  cort_thickness(seg, gray, white,
                            ...)
}