.get_transforms = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", ...) {
  fixed = check_ants(fixed)
  if (is.null(transformlist) && is.null(moving)) {
    stop("transformlist or moving must be specified")
  }
  if (!is.null(transformlist) && !is.null(moving)) {
    stop("Only one of transformlist or moving must be specified")
  }  
  if (!is.null(transformlist)) {
    if (is.character(transformlist)) {
      if (!any(grepl("arp", transformlist))) {
        warning("May not have any warp transformations")
      }
    }
    transformlist = checkimg(transformlist)
  }
  
  if (!is.null(moving)) {
    outfile = tempfile(fileext = ".nii.gz")
    reg = registration(
      filename = moving,
      template.file = fixed, 
      typeofTransform = typeofTransform,
      outfile = outfile,
      interpolator = "Linear", retimg = FALSE, correct = FALSE,
      skull_strip = FALSE, remove.warp = FALSE,
      ...)
    transformlist = reg$fwdtransforms[1]
    transformlist = checkimg(transformlist)
  }  
  L = list(fixed = fixed, 
           transformlist = transformlist)
  return(L)
}

.jacobian_image = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", 
  doLog = FALSE, 
  geom = FALSE, ...) {
  L = .get_transforms(fixed = fixed, 
                      transformlist = transformlist, 
                      moving = moving, 
                      typeofTransform = typeofTransform, ...)
  fixed = L$fixed
  transformlist = L$trantransformlist
  res = ANTsR::createJacobianDeterminantImage(
    domainImg = fixed,
    tx = transformlist,
    doLog = doLog,
    geom = geom)
  L = list(image = res)
  L$transformlist = transformlist
  L$fixed = fixed
  return(L)
}


#' Image of the Determinant of the Jacobian
#'
#' @param fixed Fixed image in a registration
#' @param transformlist List of transformation of moving image to
#' fixed image.  Only one of \code{transformlist} or \code{moving} 
#' should be specified.
#' @param moving Image to transform into \code{fixed} space, if not 
#' done previously. Only one of \code{transformlist} or \code{moving} 
#' should be specified.
#' @param typeofTransform If registration not done previously,
#' then transform to use for transformation.  Should be non-linear.
#' @param ... additional arguments to pass to \code{\link{registration}}
#' 
#' @return An image of the determinant of the Jacobian, the
#' transform list, and the fixed image.
#' @export
#' @importFrom ANTsR createJacobianDeterminantImage
#' @examples 
#' library(extrantsr)
#' library(ANTsR)
#' fi<-antsImageRead( getANTsRData("r16") ,2)
#' mi<-antsImageRead( getANTsRData("r64") ,2)
#' fi<-resampleImage(fi,c(128,128),1,0)
#' mi<-resampleImage(mi,c(128,128),1,0)
#' jac = jacobian_image(fixed = fi, moving = mi, verbose = FALSE)
jacobian_image = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", ...) {
  L = .jacobian_image(
    fixed = fixed, 
    transformlist = transformlist, 
    moving = moving, 
    typeofTransform = typeofTransform, 
    doLog = FALSE,
    geom = FALSE, 
    ...)
  return(L)
}

#' @rdname jacobian_image
#' @export
log_jacobian_image = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", ...) {
  L = .jacobian_image(
    fixed = fixed, 
    transformlist = transformlist, 
    moving = moving, 
    typeofTransform = typeofTransform, 
    doLog = TRUE,
    geom = FALSE, 
    ...)
  return(L)
}

#' @rdname jacobian_image
#' @export
geom_jacobian_image = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", ...) {
  L = .jacobian_image(
    fixed = fixed, 
    transformlist = transformlist, 
    moving = moving, 
    typeofTransform = typeofTransform, 
    doLog = FALSE,
    geom = TRUE, 
    ...)
  return(L)
}

#' @rdname jacobian_image
#' @export
log_geom_jacobian_image = function(
  fixed, 
  transformlist = NULL, 
  moving = NULL, 
  typeofTransform = "SyN", ...) {
  L = .jacobian_image(
    fixed = fixed, 
    transformlist = transformlist, 
    moving = moving, 
    typeofTransform = typeofTransform, 
    doLog = TRUE,
    geom = TRUE, 
    ...)
  return(L)
}