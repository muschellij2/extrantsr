#' @export
#' @rdname ortho2
#' @aliases ortho2,niftiImage,ANY,ANY-method
setMethod(
  "ortho2", 
  c(x = "niftiImage", y = "ANY", pdim = "ANY"),
  function(
    x, y = NULL,
    ....,
    pdim = NULL,             
    ...) {
    
    if (is.null(pdim)) {
      pdim = c(1, RNifti::pixdim(x))
      if (length(pdim) <= 3) {
        pdim = c(pdim, rep(1, 4 - length(pdim)))
      }
    }
    ortho2(x = x, y = y, 
                      pdim = pdim, 
                      ... = ...)
  }
)