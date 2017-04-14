#' @title Within Visit Registration
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param fixed filename of fixed image to be registered to.
#' @param moving filenames (or nifti) of images to register to fixed image
#' @param outfiles Output filenames, same length as moving
#' @param typeofTransform Transformation of moving to fixed image
#' @param interpolator Interpolation to be done
#' @param ... additional arguments to \code{\link{registration}}
#' @export
#' @return List of results from \code{\link{registration}}
within_visit_registration <- function(fixed, # filename of T1 image
                    moving,
                    outfiles = NULL, 
                    typeofTransform = "Rigid",
                    interpolator = "Linear",
                    ...
){
  moving = as.list(moving)
  
  # Expanding paths for ANTsR - checkimg does expansion  
  moving = sapply(moving, checkimg)
  
  if (is.null(outfiles)) {
    outfiles = sapply(seq_along(moving),
                      function(x){
                        tempfile(fileext = ".nii.gz")
                      })
  }
  n.moving = length(moving)
  if (n.moving != length(outfiles)) {
    message("# Output images do not equal input images\n")
    message(paste0("Moving", n.moving, " outfiles", length(outfiles)))
    message("# Moving Images\n")
    message(moving, sep = "\n")
    message("# Outfiles\n")
    message(outfiles, sep = "\n")
    stop("Check inputs")
  }
  dotsL = list(...)
  correct = dotsL$correct
  correction = dotsL$correction
  if (!is.null(correct)) {
    if (is.null(correction)) {
      stop(paste0("If correct is given to within_visit_registration ",
                  "then correction must be specified!"))
    }
    if (correct) {
      fixed = bias_correct(file = fixed, correction = correction)
    }
  }
  f.img = checkimg(fixed, ...)
  L = vector(mode = "list", length = n.moving)
  names(L) = names(moving)
  
  for (iimg in seq(n.moving)) {
    m.img = checkimg(moving[iimg], ...)
    ll = registration(filename = m.img, 
                  template.file = f.img, 
                  typeofTransform = typeofTransform,
                  interpolator = interpolator,
                  outfile = outfiles[iimg],
                  remove.warp = TRUE,
                  ...)
    L[[iimg]] = ll
  }
  if (n.moving == 1) {
    L = L[[1]]
  }
  return(L)
  
}


