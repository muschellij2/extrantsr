#' @docType methods
#' @rdname ortho2-methods
#' @aliases ortho2,antsImage,ANY,ANY-method
#' @title Orthographic Display, added options
#' @description Wrapper for of \code{fslr}'s \code{\link{ortho2}} function 
#' to work with antsImages
#' @return NULL
#' @param x is an object of class \code{antsImage} or similar.
#' @param y is an object of class \code{antsImage} or similar for the overlay.
#' @param pdim Pixel dimensions if passing in arrays.  
#' @param ... other arguments to the image function may be provided here.
#' @note This will NOT do \code{\link{ants2oro}} to plot the image.  
#' If that is desired, users will have to do that before calling 
#' \code{ortho2}.
#' @export
setMethod("ortho2", signature = signature(x = "antsImage", 
                                y = "ANY",
                                pdim = "ANY"), 
          function(x, y = NULL, pdim = NULL, ...) {
  if (is.antsImage(x)) {
    pdim = c(1, antsGetSpacing(x))
    x = as.array(x)
  }
  if (!is.null(y)) {
    if (is.antsImage(y)) {
      y = as.array(y)
    }
  }
  fslr::ortho2(x = x, y = y, pdim = pdim, ...)
}
)