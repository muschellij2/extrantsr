#' @title OASIS Processing Pipeline
#'
#' @description This function performs registration to a T1 template
#' using ANTsR and SyN transformation
#' @param fixed filename of fixed image to be registered to.
#' @param moving filenames (or nifti) of images to register to fixed image
#' @param outfiles Output filenames, same length as moving
#' @param typeofTransform Transformation of moving to fixed image
#' @param interpolator Interpolation to be done
#' @param ... additional arguments to \code{\link{ants_regwrite}}
#' @export
#' @return List of resutls from \code{\link{registration}}
within_visit_registration <- function(fixed, # filename of T1 image
                    moving,
                    outfiles, 
                    typeofTransform = "Rigid",
                    interpolator = "Linear",
                    ...
){
  moving = as.list(moving)
  
  # Expanding paths for ANTsR - checkimg does expansion  
  moving = sapply(moving, checkimg)
  
  
  n.moving = length(moving)
  if (n.moving != length(outfiles)){
    cat("# Output images do not equal input images\n")
    cat(paste0("Moving", n.moving, " outfiles", length(outfiles)))
    cat("# Moving Images\n")
    cat(moving, sep="\n")
    cat("# Outfiles\n")
    cat(outfiles, sep= "\n")
    stop("Check inputs")
  }
  
  f.img = checkimg(fixed, ...)
  L = NULL
  for (iimg in seq(n.moving)){
    m.img = checkimg(moving[iimg], ...)
    ll = registration(filename = m.img, 
                  template.file = f.img, 
                  typeofTransform = typeofTransform,
                  interpolator = interpolator,
                  outfile = outfiles[iimg],
                  remove.warp = TRUE,
                  ...)
    L = c(L, ll)
  }
  return(L)
  
}


