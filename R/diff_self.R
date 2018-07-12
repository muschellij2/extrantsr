#' @title Flipped Difference Image
#' @description This function flips the image over the left/right axis, then registers 
#' the flipped image to the original image and then takes a difference
#' @param img Object of class \code{nifti}, \code{ANTsR}, or \code{character}
#' @param swapdim Should the image be reoriented to RPI then flipped, registered,
#' then changed back?
#' @param verbose Print diagnostic messages
#' @param reproducible Sets the seed and 
#' \code{Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)}.
#'  See
#' \url{https://github.com/ANTsX/ANTs/wiki/antsRegistration-reproducibility-issues}
#' for discussion.
#' @param seed will execute 
#' \code{Sys.setenv(ANTS_RANDOM_SEED = seed)} before
#' running to attempt a more reproducible result.   If \code{NULL}, will not set anything, 
#' but \code{reproducible} must be \code{FALSE}.    
#' 
#' @return Object of class \code{nifti}
#' @importFrom fslr rpi_orient reverse_rpi_orient
#' @export
diff_self <- function(img, 
                      swapdim = TRUE,
                      verbose = TRUE,
                      reproducible = TRUE,
                      seed = 1
                      ){
  
  if (reproducible) {
    if (is.null(seed)) {
      stop("For reproducible = TRUE, you must set a seed!")
    }
    Sys.setenv(ANTS_RANDOM_SEED = seed)    
    itk_threads = Sys.getenv("ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS")
    Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = 1)
    on.exit({
      Sys.setenv(ITK_GLOBAL_DEFAULT_NUMBER_OF_THREADS = itk_threads)
    })
  }   
  img = checkimg(img)
  if (swapdim) {
    if (verbose){
      message("Making RPI orientation")
    }
    L = rpi_orient(file = img)
    img = checkimg(L$img)
  }
  
  rotate.img = checkimg(img)
  original.img = check_ants(img)
  
  ofile = tempfile(fileext = ".nii.gz")
  res = fslswapdim(rotate.img,
                    a = "-x", 
                    retimg = FALSE,
                    outfile = ofile,
                    verbose = verbose)
  res = NULL
  rotate.img = check_ants(ofile)
  mytx <- antsRegistration(
    fixed = original.img,
    moving = rotate.img,
    typeofTransform = "SyN",
    verbose = verbose)
  flipped.reg <- antsApplyTransforms(
    fixed = original.img,
    moving = rotate.img,
    transformlist = mytx$fwdtransforms)
  
  img.return <- original.img - flipped.reg
  if (swapdim) {
    if (verbose){
      message("Reverse RPI orientation")
    }
    file = checkimg(img.return)
    file = reverse_rpi_orient(file = file, 
                           convention = L$convention,
                           orientation = L$orientation,
                           verbose = verbose)
    return(file)  
  } else {
    img.return <- ants2oro(img.return)
  }
  return(img.return)
}