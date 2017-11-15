#' @title Perona-Malik Anisotropic Filter
#'
#' @description Wrapper for \code{oMath("PeronaMalik")}
#' 
#' @param img Object of class \code{nifti} 
#' @param n_iter number of iterations
#' @param conductance From ITK: The conductance parameter controls the 
#' sensitivity of the conductance term in the basic anisotropic diffusion equation
#' @param ... Additional arguments passed to \code{\link{iMath}}
#' @param retfile logical to indicate if an \code{antsImage} should be returned
#' (useful for chaining)
#' @export
#' @return Object of class \code{nifti}
perona_malik = function(img, n_iter, conductance, ..., retfile = FALSE) {
  res = oMath(img = img, operation = "PeronaMalik", n_iter, 
              conductance, ..., retfile = retfile)
  return(res)
}