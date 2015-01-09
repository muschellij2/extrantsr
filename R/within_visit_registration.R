#' @title OASIS Processing Pipeline
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param fixed filename of fixed image to be registered to.
#' @param moving filenames (or nifti) of images to register to fixed image
#' @param typeofTransform Transformation of moving to fixed image
#' @param interpolator Interpolation to be done
#' @param ... additional arguments to \code{\link{ants_regwrite}}
#' @export
#' @return NULL 
within_visit_registration <- function(fixed, # filename of T1 image
                    moving,
                    outfiles, 
                    typeofTransform = "Rigid",
                    interpolator = "Linear",
                    ...
){
  
  n.moving = length(moving)
  stopifnot(n.moving == length(outfiles))
  
  f.img = checkimg(fixed[[1]], ...)
  for (iimg in seq(n.moving)){
    m.img = checkimg(moving[[iimg]], ...)
    ants_regwrite(filename = m.img, 
                  template.file = f.img, 
                  typeofTransform = typeofTransform,
                  interpolator = interpolator,
                  outfile = outfiles[[iimg]],
                  ...)
  }
  return(invisible(NULL))
  
}


